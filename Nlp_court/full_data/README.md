# Clean the 854 cases files

## Process Code

| `.ipynb` notebook          | Description                                              |
| -------------------------- | -------------------------------------------------------- |
| `Full Dataset Merge.ipynb` | Merge the 854 cases dataset                              |
| `Edge and Node List.ipynb` | Create edge and node list                                |
| `Full Extractions.ipynb`   | Extract author, judge panel, opinion text                |
| `Clean Opinion Text.ipynb` | Remove references and special characters in opinion text |



## Intermediate Dataset

| Dataset                 | Description                                                  |
| ----------------------- | ------------------------------------------------------------ |
| `0_amy_cases.json`      | large dictionary {file name: raw text} for 854 cases, from Lilian's PDF parsing |
| `0_full_name_text.json` | convert `amy_cases.json` key value pair to two list: `file_name`, `raw_text` |
| `cite_edge.csv`         | edge list of citation                                        |
| `cite_node.csv`         | node list contains `case_code`, `case_name`, `court_from`, `court_type` |
| `extraction854.csv`     | full extractions include `case_code`, `case_name`, `court_from`, `court_type`, `result`, `author`, `judge_panel` |
| `decision_text.json`    | json file include `author`, `decision`(result of the case), `opinion` (opinion text), `cleaned_text` (cleaned opinion text) |
| `cleaned_text.csv`      | csv file contains allt the cleaned text                      |

