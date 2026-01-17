#' @keywords internal
APP_CSS <- shiny::HTML("
:root{
  /* Brand + primitives */
  --brand:        #FF7A00;   /* VALD primary */
  --brand-tint:   #FAEBE1;   /* light orange/tint */
  --brand-soft:   #FAEDE1;   /* softer orange bg */
  --text-main:    #343435;
  --text-strong:  #3D464D;   /* grey 500 */
  --text-muted:   #606A73;   /* grey 400 */
  --border-subtle:#E6EAED;   /* grey 50 */
  --border-soft:  #EBEFF2;   /* grey 30 */
  --bg-soft:      #f5f6f7;   /* grey 10 */
  --bg-card:      #FFFFFF;

  --success:      #549963;   /* green 500 */
  --success-soft: #DFF2E3;   /* green 50 */
  --danger:       #CC4158;   /* red 500 */
  --danger-soft:  #FAE6E9;   /* red 50 */
  --info:         #62A6E5;   /* blue 500 */
  --info-soft:    #E1EEFA;   /* blue 50 */

  --radius-lg:    18px;
  --radius-md:    12px;
  --shadow-soft:  0 18px 50px rgba(15,23,42,.06);
  --shadow-hover: 0 20px 60px rgba(15,23,42,.10);
}

/* Base layout */

body{
  font-family:'Roboto', -apple-system,BlinkMacSystemFont,'Segoe UI',Helvetica,Arial,sans-serif;
  background:var(--bg-soft);
  color:var(--text-main);
}

main.container-fluid{
  padding-top:16px;
  padding-bottom:24px;
}

/* Cards / shell */

.card-like{
  border-radius:var(--radius-lg);
  box-shadow:var(--shadow-soft);
  background:var(--bg-card);
  padding:16px 18px;
  margin-bottom:18px;
  border:1px solid var(--border-subtle);
  transition:box-shadow .18s ease, transform .18s ease;
}

.card-like:hover{
  box-shadow:var(--shadow-hover);
  transform:translateY(-1px);
}

/* App header */

.app-header{
  display:flex;
  align-items:flex-end;
  justify-content:space-between;
  gap:12px;
  margin-bottom:10px;
}

.power-title{
  font-weight:900;
  font-size:28px;
  letter-spacing:0.02em;
  background:linear-gradient(90deg,#FF7A00,#F29948);
  -webkit-background-clip:text;
  background-clip:text;
  color:transparent;
  margin:0;
}

.app-subtitle{
  font-size:13px;
  color:var(--text-muted);
  margin-top:2px;
}

/* Sidebar / data import */

.bslib-sidebar-layout .sidebar{
  background:transparent;
}

/* Section titles: Data import / Filters */
.data-import-section-title{
  font-size:13px;
  font-weight:700;
  text-transform:uppercase;
  letter-spacing:0.08em;
  color:#4b5563;
  margin-top:8px;
  margin-bottom:8px;
}

.data-import-card{
  padding:18px 16px 16px;
}

.data-import-mode{
  background:var(--bg-soft);
  border-radius:var(--radius-md);
  padding:6px 10px 2px;
  margin-bottom:12px;
}

.data-import-fields .form-group{
  margin-bottom:10px;
}

/* Primary CTA: Load CSVs */

#load_folder.btn,
#load_folder{
  width:100%;
  margin-top:4px;
  padding-top:8px;
  padding-bottom:8px;
  font-weight:600;
}

/* Buttons */

.btn{
  border-radius:9999px;
  font-weight:500;
  padding-inline:14px;
  transition:background-color .18s ease,border-color .18s ease,
             box-shadow .18s ease,transform .18s ease;
}

.btn-primary{
  background:var(--brand);
  border-color:var(--brand);
  box-shadow:0 8px 18px rgba(255,122,0,0.25);
  color:#FFFFFF;
}

.btn-primary:hover,
.btn-primary:focus{
  background:#e66e00;
  border-color:#e66e00;
  box-shadow:0 10px 22px rgba(255,122,0,0.32);
  transform:translateY(-0.5px);
}

.btn-outline-secondary{
  border-radius:9999px;
  border-color:var(--border-soft);
  color:var(--text-muted);
  background-color:var(--chip-bg, #f9fafb);
}

.btn-outline-secondary:hover{
  border-color:var(--text-muted);
  color:var(--text-strong);
}

/* File upload */
.data-import-fields .shiny-input-container {
  margin-bottom: 8px;
}

/* Stack vertically, but reverse DOM order so input is on top, button on bottom */
.data-import-fields .shiny-input-container .input-group {
  display: flex;
  flex-direction: column-reverse; /* button in DOM first, but shown last */
  gap: 4px;
}

/* White text box: 'No file selected' */
.data-import-fields .shiny-input-container
  .input-group > .form-control[readonly] {
  width: 100%;
  border-radius: var(--radius-md);
  border-color: var(--border-subtle);
  font-size: 12px;
  padding: 6px 10px;
  height: auto;
}

/* Orange Browse button, visually same as Load CSVs */
.data-import-fields .shiny-input-container
  .input-group .btn-file {
  display: block;
  width: 100%;
  margin-top: 4px;
  padding-top: 8px;
  padding-bottom: 8px;
  font-weight: 600;
  border-radius: 9999px !important;
  background: var(--brand);
  border-color: var(--brand);
  color: #ffffff;
  box-shadow: 0 8px 18px rgba(255,122,0,0.25);
  transition: background-color .18s ease,
              box-shadow .18s ease,
              transform .18s ease;
}

.data-import-fields .shiny-input-container
  .input-group .btn-file:hover,
.data-import-fields .shiny-input-container
  .input-group .btn-file:focus {
  background: #e66e00;
  border-color: #e66e00;
  box-shadow: 0 10px 22px rgba(255,122,0,0.32);
  transform: translateY(-0.5px);
}

/* Label for 'ForceDecks CSVs' in upload mode */
.data-import-fields .shiny-input-container label.control-label {
  font-size: 12px;
  font-weight: 500;
  color: var(--text-strong);
}


/* Sidebar-specific sizing */

.bslib-sidebar-layout .sidebar{
  font-size:12px;
}

.bslib-sidebar-layout .sidebar label,
.bslib-sidebar-layout .sidebar .control-label{
  font-size:12px;
}

.bslib-sidebar-layout .sidebar .form-control,
.bslib-sidebar-layout .sidebar .selectize-input{
  font-size:12px;
  padding-top:4px;
  padding-bottom:4px;
}

/* Date range: stack vertically in sidebar */
.bslib-sidebar-layout .sidebar .input-daterange{
  display:flex;
  flex-direction:column;
  align-items:stretch;
  gap:4px;
}

.bslib-sidebar-layout .sidebar .input-daterange input{
  font-size:12px;
  padding-top:4px;
  padding-bottom:4px;
  width:100%;
}

.bslib-sidebar-layout .sidebar .input-daterange .input-group-addon,
.bslib-sidebar-layout .sidebar .input-daterange .input-group-text{
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;

  border: none !important;
  background: transparent !important;
  border-radius: 0 !important;
  box-shadow: none !important;
  font-size: 11px;
  color: var(--text-muted);
  padding: 0;
  margin: 0;
}

@media (max-width: 1400px){
  .bslib-sidebar-layout .sidebar{
    font-size:11px;
  }

  .bslib-sidebar-layout .sidebar .form-control,
  .bslib-sidebar-layout .sidebar .selectize-input,
  .bslib-sidebar-layout .sidebar .input-daterange input{
    font-size:11px;
  }
}

/* Form controls */

.selectize-input,
.form-control{
  border-radius:var(--radius-md) !important;
  border:1px solid var(--border-subtle);
  box-shadow:none !important;
  background-color:#FFFFFF;
  font-size:13px;
}

.selectize-input.focus,
.form-control:focus{
  border-color:var(--brand);
  box-shadow:0 0 0 1px rgba(255,122,0,0.25) !important;
}

label,
.control-label{
  font-size:13px;
  font-weight:500;
  color:var(--text-strong);
}

/* Tabs / navset_card_tab */

.nav-tabs{
  border-bottom:none;
}

.nav-tabs>.nav-item>.nav-link{
  font-weight:600;
  border:0;
  color:#4b5563;
  padding:8px 14px;
  border-radius:9999px;
  background:transparent;
  margin-right:4px;
}

.nav-tabs>.nav-item>.nav-link:hover{
  background:var(--info-soft);
  color:var(--text-strong);
}

.nav-tabs>.nav-item>.nav-link.active{
  background:linear-gradient(90deg,var(--brand-soft),#FFEAD6);
  color:#111827;
}

/* Helper text */

.small-note{
  color:var(--text-muted);
  font-size:12px;
}

/* KPI tiles */

.kpi{
  display:grid;
  grid-template-columns:repeat(4,1fr);
  gap:14px;
}

.kpi .tile{
  background:var(--bg-card);
  border-radius:16px;
  box-shadow:var(--shadow-soft);
  padding:14px;
  border:1px solid var(--border-subtle);
}

.kpi .val{
  font-size:24px;
  font-weight:900;
}

.kpi .lbl{
  font-size:12px;
  color:var(--text-muted);
  margin-top:4px;
}

/* Select size polish: make multi + single look consistent */

.selectize-control.single .selectize-input,
.selectize-control.multi  .selectize-input {
  min-height: 32px;
  padding-top: 4px;
  padding-bottom: 4px;
}

/* Ensure the little caret icon is visible */
.selectize-control .selectize-input:after {
  border-style: solid;
  border-width: 4px 4px 0 4px;
  border-color: #9ca3af transparent transparent transparent;
  right: 10px;
  margin-top: -2px;
}

.table-legend{
  font-size:12px;
  color:var(--text-muted);
  background:var(--bg-soft);
  border-radius:var(--radius-md);
  padding:6px 10px;
  margin-top:8px;
}

.table-legend{
  font-size:12px;
  color:var(--text-muted);
  background:var(--bg-soft);
  border-radius:var(--radius-md);
  padding:6px 10px;
  margin-top:8px;
}
")
