import json
import pandas as pd

# Get the parameters from the parameter input file
def get_parameters(parameter_file):

    with open(parameter_file) as file:
        params = json.load(file)

    return params

# Eliminate records
def eliminate(input_file, elimination_file, columns_redrec):

    # Read the input file and the elimination file into pandas dataframes
    df_input = pd.read_csv(input_file)
    df_elimination = pd.read_csv(elimination_file)

    # Remove duplicate records
    df_input.drop_duplicates(subset=columns_redrec, inplace=True)

    # Set the indices of the dataframes
    student_ids = [row['student_id'] for _, row in df_elimination.iterrows()]
    # Remove records that are in the elimination dataframe
    input_columns = [column for column in df_input.columns]
    df_output = pd.DataFrame(columns=input_columns)

    # df_output.drop(df_output.index, inplace=True)
    for _, row in df_input.iterrows():
        if row['student_id'] not in student_ids:
            df_output.loc[len(df_output)] = row

    # Convert the dataframe with the remaining records to a csv file
    df_output.to_csv('output/OutputElimination.csv', index=False)

########
# MAIN #
########

parameter_file = 'Elimination_Parameters.txt'

params = get_parameters(parameter_file)

eliminate(input_file=params['input_file'],
          elimination_file=params['elimination_file'],
          columns_redrec=params['columns_redrec'])