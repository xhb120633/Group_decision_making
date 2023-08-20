# Data Documentation

This document provides details on the datasets and columns to assist with replicating our research or conducting your own analyses.

## Data Structure

The data is preprocessed and formatted in a long table structure. We've ensured the anonymity of participants by excluding raw data that contains sensitive personal information.

### Behavioral Information

Our experiments capture various behavioral aspects:

- **choice** (integer: 1,2,3,4)
  - Represents participant choices in their specific versions.
  - Note: Due to 'Latin-counter-balance', choice variable meanings differ across participant versions. 
  - To determine the option in the standard IGT, refer to `chose_performance`.
  
- **chose_performance** (integer: 11, 12, 13, 14)
  - Represents the chosen option in the standard IGT.

- **reward information**
  - Contains `gain`, `loss`, and `net-income`.
  - In IGT, both gain and loss are displayed, with net-income calculated as `gain - loss`.

- **Reaction Time (RT)** and **Confidence**
  - Records the time participants took to make each decision.
  - Confidence is measured on a scale of 1-7 (from least to most confident).

- **Group Conditions**
  - Captures social influence through the distribution of group votes (e.g., 3,2,0,0).
  - Important: Recorded choices are task-specific, not standardized. Transformation to standard IGT may be required.
  - Conversion codes are provided in `reformat.m` for modeling and `Figure2.R` for model-free analysis.

- **Demographics**
  - Contains gender (1 for Female, 2 for Male) and age.

## Dataset Specifics

### `Individual_data.mat`
Data collected during individual conditions.

Columns: `Sub_id`, `Trial_Number`, `Original Choice`, `Choice_RT`, `Confidence`, `Confidence_RT`, `Gain`, `Loss`, `Net_income`, `Total_score (starting 2000)`, `Chose_performance`, `Times participants have selected for each deck (12-15)`, `version`, `gender`, `age`.

### `No_leader_data.mat`
Data from group conditions, excluding leader data.

Columns: `Sub_id`, `Trial_Number`, `Individual decision`, `Choice_RT`, `Confidence`, `Confidence_RT`, `Gain`, `Loss`, `Net_income`, `Total_score (starting 2000)`, `Chose_performance (individual_decision)`, `Group_decision`, `Chose_performance(group_decision)`, `distribution of the group votes (14-17)`, `Times participants have selected for each deck (18-21)`, `version`, `gender`, `age`.

### `Leader_data.mat`
Data from group conditions, focusing on leader data.

Columns: As per `No_leader_data.mat`, with additional columns for leader decisions, confidence, reaction time, and distribution of group votes.

## Participant Conditions

Determine participant conditions using their subject ID:

- `1000 < sub_id < 2000`: no_leader_no_discussion condition
- `2000 < sub_id < 3000`: no_leader_discussion condition
- `3000 < sub_id < 4000`: leader_no_discussion condition
- `sub_id > 4000`: leader_discussion condition

**Grouping Example**: 
For a subject with ID 1101, teammates would have IDs: 1201, 1301, 1401, and 1501. Use the `group_tag.csv` for group-level analysis. In leader groups, the last column's `sub_id` denotes the leader.

## Contact

For queries related to code, data, or documentation, please email: [hanboxie1997@email.arizona.edu](mailto:hanboxie1997@email.arizona.edu).
