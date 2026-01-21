# Diagrams (PlantUML)

The `.puml` files in this folder are intended to be viewed with PlantUML.

## VS Code
- Install a PlantUML extension (e.g. *PlantUML*).
- Open a `.puml` file and use the extension preview.

## CLI (optional)
If you have PlantUML installed locally, you can render with:
- `plantuml -tpng docs/diagrams/*.puml`

(Exact command depends on how PlantUML is installed on your machine.)
Included diagrams:
- `compute_unit_microarch.puml` — Compute Unit microarchitecture (as-built RTL) ✅
- `mailbox_interconnect.puml` — Mailbox interconnect / AXI-Lite switch (UML component) 📬
- `mailbox_center.puml` — Mailbox Center / Switch / Endpoint plan (RTL-based) 📬
- `mini_control_processor.puml` — Mini Control Processor (component/ports) 🧭