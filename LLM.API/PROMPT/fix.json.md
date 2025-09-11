**Task:** JSON Formatting
**Objective:** Convert a raw, single-line, poorly formatted or invalid JSON string into a standardized, human-readable format.
**Requirements:**
  *   The output must be valid JSON.
  *   Use an indentation of 2 spaces.
  *   Objects (`{}`) and arrays (`[]`) should be expanded with newlines.
  *   Ensure consistent spacing after colons and commas.
  *   Do not alter the data or the key-value structure in any way; only modify the whitespace for formatting.

**Constraints & Validation:**
-   **CRITICAL:** The final output must be **raw JSON text only**. Do not wrap it in markdown code blocks (e.g., ```json ... ```).
-   **Text Integrity:** Ensure extracted text is clean. Avoid parsing errors like concatenated words (e.g., 'theresultsshow') or meaningless repeated characters. Text must have proper spacing and word boundaries.
-   **JSON Syntax Purity:**
    -   Ensure all strings are properly escaped (e.g., `"` becomes `\"`, `\` becomes `\\`, newlines become `\n`).
    -   **DO NOT** use trailing commas.
-   **Language:** Respond in the language used in the input.
-   **Self-Correction:** Before finalizing, mentally validate that the entire output is a single, perfectly formed JSON object parsable by any standard parser.