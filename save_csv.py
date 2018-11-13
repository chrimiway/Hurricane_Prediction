import csv

# csv: two dimentional list
def save_csv(file_name, csv_file):
    with open(file_name, "w") as csvf:
        csv_writer  = csv.writer(csvf, delimiter = ",")
        for r in csv_file:
            csv_writer.writerow(r)
            pass
        pass
    pass

