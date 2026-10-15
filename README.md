# Bio-Plex BAMA Dashboard

A Shiny dashboard that turns **Bio-Plex (Luminex) RAW `.xlsx` exports** into the long-format
**BAMA export table** (one row per well × analyte, 95 columns). It runs the processing logic of
`report_michaela_7.R` (version `2026-10-06f`) on every uploaded plate and styles the interface
after the NAb (Neut) Assay Dashboard.

```
bioplex_dashboard/
├── app.R            # the whole app (processing functions + UI + server) – one self-contained file
├── README.md
└── www/
    └── airu_logo.png   # optional – shown in the header if present
```

---

## 1. Requirements and how to run

R (≥ 4.1 recommended) and these packages:

```r
install.packages(c("shiny", "shinydashboard", "shinyjs", "DT", "plotly",
                   "readxl", "openxlsx", "dplyr", "tidyr", "stringr"))
```

Run from RStudio (open `app.R` → **Run App**) or from the console:

```r
shiny::runApp("path/to/bioplex_dashboard")
```

Uploads of up to 500 MB are accepted (`options(shiny.maxRequestSize)` in `app.R`).

---

## 2. Quick start

1. **Upload** one or more Bio-Plex RAW `.xlsx` files (sidebar → *Upload Bio-Plex RAW files*).
2. *(Optional)* upload the **shipment metadata** `.xlsx` to pre-fill `global_identifier` (see §5).
3. Check the detected analytes in the **Raw File Upload** tab and pick the **Analyte prefix**
   (e.g. `426c`). The most common prefix is suggested automatically; you can also type your own.
4. *(Optional)* adjust **Export Settings** (see §6).
5. Click **▶ Run Processing**.
6. Review in **Plate Review** and **Export Table**.
7. Download the `.xlsx` and/or `.txt` from the **Export** tab.

Changes made in *Export Settings* after a run (assignments, baseline visits, sample role, summary
category) apply **immediately** to the table and downloads – no re-run needed. Only the `study`
field and the analyte prefix require clicking **Run Processing** again.

---

## 3. Dashboard tabs

| Tab | What it does |
|---|---|
| **Overview** | Value boxes (plates, analytes, unique samples, rows), workflow guide, run summary and any warnings. |
| **Raw File Upload** | Uploaded files with plate ID, date, reader serial number, wells and analyte counts; detected analytes (✔ = selected by the prefix); raw sheet preview. |
| **Export Settings** | Study, shipment-metadata match results, `sample_role`, `summary` category, `global_identifier` / `exclude` / `isotype` assignments, baseline visits. |
| **Plate Review** | 96-well plate map per plate and analyte (MFI, background-subtracted MFI, or bead count) and a per-plate/analyte table of background (Type B) wells and bead counts. |
| **Export Table** | Searchable preview of the final table (toggle: only filled columns). |
| **Export** | Download `.xlsx` / `.txt`, run metadata, column legend. |

---

## 4. Expected RAW file format

The processing assumes the standard Bio-Plex Manager export:

- **Header block (first 7 rows, column A)** containing `File Name:`, `Reader Serial Number:` and `Plate ID:`.
  - `experiment_date` = first `_`-separated token of the file name (`YYYYMMDD`).
  - `experiment_identifier` = the full `Plate ID:` value; `plate_identifier` = its 3rd `_` token (e.g. `P1`).
- **Data table** starting at the row whose first cell is `Well`, with columns `Well`, `Type`, `Description`,
  then one column per analyte, then trailing columns (`Region`, `% Agg Beads`, `Sampling Errors`,
  `Plate ID`, `Bead Count`, `Acquisition Time`).
- **Analyte cells** look like `185,00 (50)` → MFI `185.0` (decimal comma) and bead count `50`.
- **Description** is `SampleName_VisitNumber_TitrationPoint[_PlateNumber]`. Only the first two parts are used:
  `sample_identifier` and `visit_identifier`.
- **Type** is `B` (blank/background), `C…` (control, e.g. C1) or `X…` (sample, e.g. X1).

A file with no `Well` header row, or no recognised trailing column, is reported as an error and skipped.

---

## 5. How the table is built

### Analytes
Analytes whose name (region number removed, e.g. `426c CD4bs KO (45)` → `426c CD4bs KO`) **starts with the
chosen prefix** (case-insensitive) are exported, in file order. The **`BLANK`** analyte is **always included**,
with exactly the same columns as any analyte (MFI, background-subtracted MFI, bead count, …).

### Calculated columns

| Column | Rule |
|---|---|
| `fluorescence_intensity` | MFI (value before the bracket). |
| `bead_count` | Number in brackets. |
| `fluorescence_intensity_background_subtracted` | MFI − mean MFI of the **Type `B`** wells for the same analyte on that plate. Negative values are kept. Empty (with a warning) if a plate has no `B` wells. |
| `negative_bead` | `TRUE` on **BLANK** analyte rows, `FALSE` on all other analytes. |
| `summary` | See below. |
| `sample_role` | See below. |
| `well`, `well_group` | Well position and the RAW `Type`. |
| `sample_identifier`, `visit_identifier` | Parts 1 and 2 of `Description`. |
| `experiment_date`, `experiment_identifier`, `plate_identifier`, `machine_serial_number` | From the RAW header block. |
| `dataset_creation_date` | The day the export is created. |
| `study` | Editable in Export Settings (default `301, Moore_989`). |
| fixed constants | `analysis_software` = Bio-plex Manager Software, `assay_platform` = BAMA, `bead_type` = MAGNETIC, `first_detection` = DIRECT IgG-PE, `lab` = AIRU, `network` = CAVD, `plate_type` = 96, `sample_species` = HUMAN, `sample_type` = SER, `timepoint_units` = Week, `avidity_index_calculated` = FALSE. |
| defaults (changeable) | `global_identifier` = `guspec`, `exclude` = `FALSE`, `isotype` = `IgG`, `timepoint_baseline` = `FALSE`. |

All remaining columns of the 95-column layout that cannot be derived from the RAW file are left empty.

### `sample_role` (from the RAW `Type`)
| Type | sample_role |
|---|---|
| `C` (C1, C2 …) | `CONTROL_MAB` (default) or `CONTROL` – choose in Export Settings |
| `X` (X1, X2 …) | `SAMPLE` |
| `B` | `BLANK` |

The role follows the **well's** Type, so BLANK-analyte rows take the role of their own well. Any other Type
is left empty and listed in a warning in Export Settings.

### `summary` category (Export Settings)
- **Samples** *(default)* – `individual` / `replicate` from the Type column: the first well of a Type (read in plate
  order A1, B1 … A2 …) is `individual`, later wells with the same Type are `replicate`.
- **Boolean** – every row is set to `TRUE` or `FALSE` (your choice).

### `global_identifier` from the shipment metadata
Upload the shipment `.xlsx` in the sidebar. It must contain the columns **`PID/ Sample name`**,
**`Other Spec ID`** and **`Visit number`** (matched by name, ignoring case and punctuation). A row gets
`global_identifier` = `Other Spec ID` only when **both** match:

- `sample_identifier` = `PID/ Sample name`, and
- the **number** in `visit_identifier` (`V104` → `104`) = `Visit number`.

Unmatched rows keep the default (`guspec`). Controls (no visit number) and samples missing from the
shipment file cannot match; the **Shipment metadata** box in Export Settings shows how many sample/visit
combinations matched and lists those that did not. Duplicate PID/visit rows in the metadata are reported
and the first is used.

### Assignments (Export Settings)
For **`global_identifier` / `exclude`** and for **`isotype`** you can assign a value to selected samples:

1. choose the column, type or pick the value;
2. choose the **plate(s)** it applies to (all by default, or any subset);
3. choose the **sample(s)** (the list shows samples found on the selected plates);
4. click **Add**.

Assignments appear in a table (column, value, plates, samples) where rows can be selected and removed.
Samples without an assignment keep the defaults. Precedence, lowest to highest:
defaults → shipment metadata → manual assignments (a later assignment beats an earlier one for the
same sample, plate and column). Assignments are cleared when new RAW files are uploaded.

### Baseline visits
Pick one or more `visit_identifier` values; matching rows get `timepoint_baseline = TRUE`, all others `FALSE`.

---

## 6. Plate Review

- **Plate / Analyte** selectors and a **Show** option:
  - **MFI** and **Background-subtracted MFI** – heat colours; wells with a bead count below the threshold are
    marked with a black ✕.
  - **Bead count** – 🟥 **red** if the bead count is below *Flag bead count below* (default 30), 🟩 **green**
    otherwise (a count equal to the threshold is green), grey = no bead count. A legend shows the well counts.
- If a plate/analyte has no values for the chosen metric, or the threshold box is empty, a short message is
  shown instead of a plot.
- The table below lists, per plate × analyte, the number of Type `B` wells, mean B-well MFI, mean/min bead
  count and wells below the threshold.

---

## 7. Export

- **Column order: A–Z.** All 95 columns are sorted alphabetically in both the downloads and the preview.
- **`.xlsx`** – one sheet named `export`, frozen header row, dates formatted `yyyy-mm-dd`.
- **`.txt`** – the same table, tab-delimited, empty cells for `NA`.
- The file name (without extension) is editable in the Export tab.
- The *Run Metadata* box records files processed, prefix, analytes, size, baseline visits, metadata match
  count, `sample_role` / `summary` settings and the number of assignments.

---

## 8. Validation and known notes

- Processing was validated against the reviewed example export (`CAVD301_LUM05_R1_20261006_A_1_AM_COMMENTS.xlsx`)
  by rebuilding 25 RAW files from it: all non-BLANK rows reproduce the example in every column, apart from the
  intentional changes (`negative_bead` is now a flag, `sample_role`, column order, `dataset_creation_date`).
  The app has not yet been tested on genuine Bio-Plex Manager exports, so please check the first real run against
  a trusted table and report any difference in the RAW layout.
- The shipment file in the test data matched 117 of 129 sample/visit combinations; the unmatched ones were
  controls without visit numbers and a sample (`77785979`) absent from the shipment file.
- Plates without Type `B` wells produce empty background-subtracted values (reported as a warning).
- `openxlsx` can write an unreferenced relationship entry in the sheet XML; Excel ignores it, but strict
  readers such as Python's `openpyxl` may refuse the file. Reading it with `readxl` or Excel works.

---

## 9. Troubleshooting

| Symptom | Likely cause / fix |
|---|---|
| "No analyte starts with prefix …" | Wrong prefix – use one of the suggested prefixes or check the *Analytes Detected* table. |
| A file is skipped with "Failed on …" | Not a Bio-Plex RAW layout (no `Well` header row / trailing columns); see §4. |
| Metadata "not loaded: missing column(s)" | The shipment file lacks `PID/ Sample name`, `Other Spec ID` or `Visit number`. |
| Sample / plate / visit dropdowns are empty | They fill after **Run Processing**. |
| Background-subtracted values are empty | The plate has no Type `B` wells for that analyte. |
| Header has no logo | Put `airu_logo.png` in a `www/` folder next to `app.R`. |

---

## 10. Provenance

Processing logic: `report_michaela_7.R` (2026-10-06f-ordered-cols). Interface style: NAb Assay (Neut)
Shiny dashboard (`shinydashboard`, blue skin, navy header `#1a3a6b`, dark sidebar, gold active-tab accent).
