from graphviz import Digraph

OUT_BASENAME = "/Users/kpv27/Documents/code/Research/cold_start_annotation_v3"

g = Digraph("ColdStartAnnotationV3")
g.attr(rankdir="TB", splines="spline", nodesep="0.5", ranksep="0.7",
       fontname="Helvetica", fontsize="12", labelloc="t", pad="0.2")
g.attr("node", shape="box", style="rounded,filled", fontname="Helvetica",
       fontsize="12", margin="0.15,0.12", penwidth="1.2")
g.attr("edge", arrowhead="normal", arrowsize="0.9", penwidth="1.1")

blue  = "#CFE8FF"
gray  = "#E5E7EB"
yellow= "#FFF3C4"
pink  = "#FFD6E7"
green = "#CFF4D2"
note  = "#F5F5F5"

g.node("Goal",
       "Goal:\nCold-start LLM annotation with human refinement\nFocus: near hits/misses + reduce bias",
       fillcolor=blue)

g.node("Prep",
       "Preparation:\n"
       "- Define constructs and dimensions\n"
       "- Run multiple LLMs on a subset\n"
       "- Identify disagreements (near hits/misses)\n"
       "- Select annotated dataset for hold-out eval",
       fillcolor=gray)

g.node("P1",
       "Phase 1 — Iterative Prompt Development:\n"
       "- Human refines prompts on a small subset\n"
       "- Review LLM outputs; establish baseline",
       fillcolor=yellow)

g.node("P2",
       "Phase 2 — Structured Near-Miss/Hit Refinement:\n"
       "- Present edge cases (LLM disagreement) + random cases\n"
       "- Blind human review (agree / disagree)\n"
       "- Add curated examples (hits / near hits / near misses)\n"
       "- Adjust prompts / roles / self-verification",
       fillcolor=pink)

g.node("Loop",
       "Iteration Criterion:\nRepeat until at least one configuration performs adequately",
       style="dashed,rounded,filled", fillcolor=note)

g.node("Eval",
       "Final Evaluation:\n"
       "- Run on full annotated dataset\n"
       "- Compare to ground truth\n"
       "- Metrics: agreement, edge-case precision/recall, bias reduction",
       fillcolor=green)

g.node("Out",
       "Expected Outcome:\nValidated human-in-the-loop cold-start method; robust annotation pipeline",
       fillcolor=blue)

g.edge("Goal", "Prep")
g.edge("Prep", "P1")
g.edge("P1", "P2")
g.edge("P2", "Loop")
g.edge("Loop", "P2", label=" refine ", fontsize="10")
g.edge("Loop", "Eval")
g.edge("Eval", "Out")

g.render(OUT_BASENAME, format="png", cleanup=True)
g.render(OUT_BASENAME, format="svg", cleanup=True)
