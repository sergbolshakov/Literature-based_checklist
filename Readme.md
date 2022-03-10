The repository contains scripts for creating a fungal checklist (names of taxa grouped by synonymy, supplemented with data on the distribution of these taxa across administrative regions and bibliographic citations where these records were published) from a literature database.

A literature database must satisfy the conditions for correct work of the scripts:

- the structure of the data table should correspond to the scheme of fields described [here](https://github.com/sergbolshakov/Literature_data_schema) (in Russian);
- the data table should be accompanied by the reference table synchronized with Zotero / Juris-M library - the table should contain the fields of internal identifiers of records in the library (Zotero Key) and the scanned citations (the [RTF/ODF-Scan add-on](https://zotero-odf-scan.github.io/zotero-odf-scan/) should be installed);
- the nomenclature table with information about taxon names should have in addition to scientific name and accepted name fields also fields indicating the taxonomic status of the name (accepted, synonym, or misapplied) and protonym to determine homotypic synonyms. To italicise synonyms, you also need to break down the scientific name into its component parts (generic name, specific epithet, authorship information). See the [list of fields](data/nomenclator_template.tsv) of the nomenclator table used in the project.

The scripts output the checklist as document in `.docx` format.

There are still shortcomings in the formatting of the text at the moment. Several replacements will need to be made :

- `.^p` replace with `.^p^p` — add a space between paragraphs;
- `^l` replace with `^p` — replace the soft breaks with line endings;
- `^p^pOn ` replace with `^pOn ` and format font as Regular — remove the spaces between the accepted names and the substrata if there are no synonyms;
- `^13^13<[A-Z]*> <[a-z]*>` (use wildcards) replace with format font as Bold, Italic — italicise the binomials in the accepted names.
