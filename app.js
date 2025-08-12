import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

import LLM from "../../shared/llm/llm.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export const lambdaHandler = async (event, context) => {
  const inputDir =
    event.inputDir || path.join(__dirname, "../../localData/analysis");
  const outputDir =
    event.outputDir || path.join(__dirname, "../../localData/output");
  const outputPath = path.join(outputDir, "annotation_quality_check.csv");

  try {
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir, { recursive: true });
    }

    const promptsPath = path.join(__dirname, "prompts.json");
    const prompts = JSON.parse(fs.readFileSync(promptsPath, "utf8"));
    const qualityCheckPrompt = prompts[0];

    console.log(`Input directory: ${inputDir}`);
    console.log(`Output file: ${outputPath}`);

    const files = fs
      .readdirSync(inputDir)
      .filter((file) => file.endsWith(".json"))
      .sort();

    console.log(`total input files=${files.length}`);

    let allResults = [];
    let stats = {
      filesProcessed: 0,
      existingAnnotations: 0,
      accurateAnnotations: 0,
      inaccurateAnnotations: 0,
      qualityChecks: 0,
    };

    for (const file of files) {
      const filePath = path.join(inputDir, file);
      const sessionData = JSON.parse(fs.readFileSync(filePath, "utf8"));

      console.log(`${file}: ${sessionData.transcript.length} utterances`);

      const annotatedUtterances = sessionData.transcript.filter(
        (utterance) => utterance.annotations && utterance.annotations.length > 0
      );

      if (annotatedUtterances.length === 0) {
        console.log(`no annotations found in ${file}, skipping...`);
        continue;
      }

      console.log(
        `total annotated utterances to check=${annotatedUtterances.length}`
      );

      const dialogueContext = annotatedUtterances
        .map((utterance) => {
          const annotationText = utterance.annotations
            .map((ann) => `ANNOTATION: ${ann.teacherMove} - "${ann.reasoning}"`)
            .join("; ");

          return `${utterance._id}. ${utterance.role}: "${utterance.content}" [${annotationText}]`;
        })
        .join("\n");

      const checkPrompt = `${qualityCheckPrompt.prompt}

## Annotated Utterances to Review:
${dialogueContext}

Please assess each existing annotation for accuracy.`;

      try {
        const llm = new LLM({ quality: "medium" });

        llm.addUserMessage(checkPrompt, {
          conversation: dialogueContext,
        });

        const llmResponse = await llm.createChat();
        console.log(`LLM quality check response for ${file}:`, llmResponse);

        let qualityChecks = [];
        try {
          if (Array.isArray(llmResponse)) {
            qualityChecks = llmResponse;
          } else if (typeof llmResponse === "object" && llmResponse !== null) {
            const possibleArrayKeys = [
              "annotations",
              "judgments",
              "reviews",
              "results",
              "annotations_reviewed",
              "annotations_to_review",
              "annotations_review",
              "assessments",
              "annotationsToReview",
            ];

            for (const key of possibleArrayKeys) {
              if (llmResponse[key] && Array.isArray(llmResponse[key])) {
                qualityChecks = llmResponse[key];
                break;
              }
            }

            // if no array found but has utterance_id, it's a single assessment
            if (qualityChecks.length === 0 && llmResponse.utterance_id) {
              qualityChecks = [llmResponse];
            }

          } else if (typeof llmResponse === "string") {
            // sometimes, llm response is a string so we need to extract json
            // from it.
            const jsonMatch = llmResponse.match(/\[[\s\S]*\]/);
            if (jsonMatch) {
              qualityChecks = JSON.parse(jsonMatch[0]);
            } else {
              qualityChecks = [];
            }
          } else {
            // weird response format/ type from llm
            qualityChecks = [];
          }
        } catch (parseError) {
          qualityChecks = [];
        }

        for (const utterance of annotatedUtterances) {
          const qualityCheck = qualityChecks.find(
            (check) => check.utterance_id === parseInt(utterance._id)
          );
          const annotation = utterance.annotations[0];

          const result = {
            session_id: file.replace(".json", ""),
            utterance_id: utterance._id,
            role: utterance.role,
            content: utterance.content,
            start_time: utterance.start_time || "",
            end_time: utterance.end_time || "",
            teacher_move: annotation.teacherMove,
            reasoning: annotation.reasoning,
            ACCURATE: qualityCheck?.qualityStatus === "ACCURATE" ? "YES" : "NO",
            INACCURATE:
              qualityCheck?.qualityStatus === "INACCURATE" ? "YES" : "NO",
            REASONING:
              qualityCheck?.qualityStatus === "INACCURATE"
                ? qualityCheck.reasoning || ""
                : "",
          };

          allResults.push(result);

          if (qualityCheck) {
            stats.qualityChecks++;
            if (qualityCheck.qualityStatus === "ACCURATE")
              stats.accurateAnnotations++;
            if (qualityCheck.qualityStatus === "INACCURATE")
              stats.inaccurateAnnotations++;
          }

          stats.existingAnnotations++;
        }
      } catch (llmError) {
        for (const utterance of annotatedUtterances) {
          const annotation = utterance.annotations[0];

          const result = {
            session_id: file.replace(".json", ""),
            utterance_id: utterance._id,
            role: utterance.role,
            content: utterance.content,
            start_time: utterance.start_time || "",
            end_time: utterance.end_time || "",
            teacher_move: annotation.teacherMove,
            reasoning: annotation.reasoning,
            ACCURATE: "NO",
            INACCURATE: "NO",
            REASONING: "LLM call failed",
          };

          allResults.push(result);
          stats.existingAnnotations++;
        }
      }

      stats.filesProcessed++;
    }

    if (allResults.length === 0) {
      throw new Error("No results to export");
    }

    const headers = [
      "session_id",
      "utterance_id",
      "role",
      "content",
      "start_time",
      "end_time",
      "teacher_move",
      "reasoning",
      "ACCURATE",
      "INACCURATE",
      "REASONING",
    ];

    const csvRows = [headers.join(",")];

    for (const result of allResults) {
      const row = headers.map((header) => {
        const value = result[header] || "";
        if (
          typeof value === "string" &&
          (value.includes(",") || value.includes('"') || value.includes("\n"))
        ) {
          return `"${value.replace(/"/g, '""')}"`;
        }
        return value;
      });
      csvRows.push(row.join(","));
    }

    fs.writeFileSync(outputPath, csvRows.join("\n"));

    const accuracyRate =
      stats.existingAnnotations > 0
        ? (
            (stats.accurateAnnotations / stats.existingAnnotations) *
            100
          ).toFixed(1)
        : "0.0";

    console.log

    return {
      statusCode: 200,
      body: JSON.stringify(summary),
    };
  } catch (error) {
    console.error("❌ Error in quality check:", error);
    return {
      statusCode: 500,
      body: JSON.stringify({
        error: "Quality check failed",
        message: error.message,
      }),
    };
  }
};
