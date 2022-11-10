The repository contains scripts for creating a fungal checklist (names of taxa grouped by synonymy, supplemented with data on the distribution of these taxa across administrative regions and bibliographic citations where these records were published) from a literature database.

A literature database must satisfy the conditions for correct work of the scripts:

- the structure of the data table should correspond to the scheme of fields described [here](https://github.com/sergbolshakov/Literature_data_schema) (in Russian);
- the data table should be accompanied by the reference table synchronized with Zotero / Juris-M library — the table should contain the fields of internal identifiers of records in the library (Zotero Key) and the scanned citations (the [RTF/ODF-Scan add-on](https://zotero-odf-scan.github.io/zotero-odf-scan/) should be installed);
- the nomenclature table with information about taxon names should have in addition to scientific name and accepted name fields also fields indicating the taxonomic rank of the name and protonymID to determine homotypic synonyms. See the [list of fields](data/nomenclator_template.tsv) of the nomenclator table used in the project.

The scripts output the checklist as document in `.docx` format. There is still one flaw in the output file - there are dots in the lines separating paragraph views. This can be removed by replacing `.^p.^p`  with `.^p^p`.
