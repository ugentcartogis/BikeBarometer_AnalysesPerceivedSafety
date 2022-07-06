import json
import pandas as pd

# Get the parameters from the parameter input file
def get_parameters(parameter_file):

    with open(parameter_file) as file:
        params = json.load(file)

    return params

# Read the csv file containing the records to clean and convert to a pandas dataframe and a grouped dataframe
def to_df(csv_file):
    df = pd.read_csv(csv_file)
    df_grouped = df.groupby(['student_id']).count()

    return df, df_grouped

# Function to check whether a record must be deleted, checked or kept
def delete_or_not(csv_file):

    # Get the dataframe and the grouped dataframe
    df, df_grouped = to_df(csv_file)

    # Set 'yes' as the default value for the elimination column
    df['Elimination'] = 'yes'

    # Run through the records of the grouped dataframe (all separate student_ids)
    for index, row in df_grouped.iterrows():

        # If there is a hard error, the row must be deleted, so pass
        if row['Hard error'] > 0:
            continue
        # If there are multiple soft errors, the row must be checked manually
        elif row['Soft error'] > 1:
            df.loc[df['student_id'] == index, ['Elimination']] = 'check'
        # if there is a single soft error, the row must be kept, but in some cases checked
        elif row['Soft error'] == 1:
            if 'importance' in df.loc[df['student_id'] == index, ['Soft error']]:
                df.loc[df['student_id'] == index, ['Elimination']] = 'check'
            else:
                df.loc[df['student_id'] == index, ['Elimination']] = 'no'

    # Convert the dataframe to a csv file
    df.to_csv('output/RecordsToDeleteOrNot_output.csv', index=False)

########
# MAIN #
########

parameter_file = 'DeleteOrNot_Parameters.txt'

params = get_parameters(parameter_file)

delete_or_not(csv_file=params['csv_file'])