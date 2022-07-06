import csv
import databaseparameters as dbp
import psycopg2
from psycopg2.extensions import AsIs
import json

# Get the parameters from the parameter input file
def get_parameters(parameter_file):

    with open(parameter_file) as file:
        params = json.load(file)

    return params

# Make a database connection
def makeConnection():
    try:
        conn = psycopg2.connect(host=dbp.host,
                                port=dbp.port,
                                database=dbp.database,
                                user=dbp.user,
                                password=dbp.password)
        return conn
    except:
        print ("I am unable to connect to the database")


# Close the database connection
def closeConnection(conn, cur=''):
    if cur != '':
        cur.close()
    conn.close()


# Read the csv file with the raw data
def read_orig_data(filename):

    with open(filename, mode='r') as input_file:
        reader = csv.reader(input_file)
        # save the header
        header = next(reader)
        # create a dictionary with the student_id as key and the record as value
        data_dict = {rows[0]: rows for rows in reader}

    return data_dict, header

# Set up the csv file to which the records to clean will be written
def set_up_output(header):
    output_file = open('output/RecordsToClean_output.csv', 'w', newline='')
    writer = csv.writer(output_file)
    writer.writerow(['Elimination', 'Imputation', 'Hard error', 'Soft error']+header)

    return writer, output_file

# Write records that must/can be cleaned to the csv file
def find_records_to_clean(orig_data_filename, table, error_definitions, regular, query, query_var):

    # Get the original data in a dictionary and get the header of the data
    data_dict, header = read_orig_data(orig_data_filename)

    # Set up the csv file to which the records to clean will be written
    writer, output_file = set_up_output(header)

    # Get the student ids of the data
    student_ids = tuple([int(key) for key in data_dict])

    # Loop over the error types
    if regular:
        for error_type in error_definitions:
            # Loop over the error definitions in the list of the current error type
            for error_def in error_definitions[error_type]:
                # Run the query
                conn = makeConnection()
                cur = conn.cursor()
                cur.execute("""SELECT student_id
                                   FROM %(table)s
                                   WHERE %(error_def)s
                                   ORDER BY student_id;""", {
                        'table': AsIs('{0}{1}{0}'.format('"', AsIs(table))),
                        'error_def': AsIs(error_def)}),

                results = cur.fetchall()
                # Loop over the resulting records of the query
                for row in results:
                    row_to_write = data_dict[str(row[0])]
                    if error_type == 'hard_errors':
                        writer.writerow(['','', error_def, '']+row_to_write)
                    elif error_type == 'soft_errors':
                        writer.writerow(['', '', '', error_def] + row_to_write)

                closeConnection(conn, cur)

    # For separate queries
    else:
        # Run the query
        conn = makeConnection()
        cur = conn.cursor()
        if query_var:
            cur.execute(query, {"student_ids" : AsIs(student_ids)}),
        else:
            cur.execute(query),
        results = cur.fetchall()
        # Loop over the resulting records of the query
        for row in results:
            row_to_write = data_dict[str(row[0])]
            writer.writerow(['', '', '', ''] + row_to_write)

        closeConnection(conn, cur)

    # Close the output file
    output_file.close()


########
# MAIN #
########

parameter_file = 'GetRecordsToClean_Parameters.txt'

params = get_parameters(parameter_file)

find_records_to_clean(orig_data_filename=params['orig_data_filename'],
                      table=params['table'],
                      error_definitions=params['error_definitions'],
                      regular=params['regular'],
                      query = params['query'],
                      query_var = params['query_var'])