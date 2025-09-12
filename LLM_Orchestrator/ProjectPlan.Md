# Orchestrating LLM Agents in the Annotation of Tutor Transcripts: A Comparative Study

## Research Questions
- Which processes of LLM Agent orchestration lead to better reliability with human raters?
- Does it help to have one LLM check another LLM’s annotations? Does it matter if the models differ?
- Does it help to have LLMs resolve disputes?
- Where is the variability in LLM annotation? Does this tell us something about how different models/orchestration methods perform?
- How many rounds of orchestration are needed to get great results?
- What type of orchestration is right for various tasks? (e.g., prompting, deidentification, etc.)

## Process
1. Human label data -- used from other projects: 
   - PCLA annotated Saga data (V2)
   - TeachFX data (V2)
2. Run each orchestration LLM agent method (defined below) with the following bootstrapping:  
   - Randomly sample 100 lines  
   - Estimate IRR (e.g., Cohen’s kappa)  
   - Repeat 25+ times  
3. Analyze and compare:  
   - IRR between humans and each method for each bootstrapped iteration  
   - Are these methods statistically different from one another?  
   - Validity (across bootstraps) by method and construct  
   - Does variability correlate with human coders’ initial IRR?

## Orchestration LLM Agent Methods
- **All Models Return All Codes** (plus reasoning for each code)  
  Models: GPT, Claude, Gemini  
  Prompt remains the same.

### Methods
1. **No Orchestration (Control)**  
   - Prompt LLM to code data (e.g., Prompt 7 MVP)  
   - Focus on providing an explanation  

2. **Self-Verification Orchestration – Same Model**  
   - One prompt for coding (worker)  
   - Another prompt (same model) checks the first model’s work  

3. **Other-Verification Orchestration – Different Models**  
   - One prompt for coding (worker)  
   - Another prompt (different model) checks the first model’s work  
   - Run for each permutation of models  

4. **Self-Manager-Resolution Orchestration – Same Model**  
   - Same prompt run twice for coding (two workers)  
   - Manager prompt (same model) checks discrepancies  

5. **Other-Manager-Resolution Orchestration – Different Models**  
   - Same prompt run twice for coding (two workers)  
   - Manager (different model) checks discrepancies  

6. **Mixed-Manager-Verification Orchestration – All Different**  
   - Same prompt run twice for coding (two workers)  
   - Manager (different model) checks discrepancies  
   - Workers and manager are all different models  

## Table 1: Orchestration Schema

| Method                          | Workers | Checkers | Managers |
|---------------------------------|---------|----------|----------|
| No Orchestration                | 1       | 0        | 0        |
| Verification Orchestration      | 1       | 1        | 0        |
| Manager-Resolution Orchestration| 2       | 0        | 1        |

**Definitions**  
- **Self**: Same model for worker/checker/manager  
- **Other**: Checker/manager is a different model from worker  
- **Mixed**: All models are different  

## Papers
- Tutor move annotation (1 round)  
- Deidentification space  
- General paper on orchestration  
