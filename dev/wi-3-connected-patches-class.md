> Part of the [Connectivity API plan](connectivity-api-plan.md). Resolution mechanism: `vignette("interpatch-distance-and-resolution")`.

# WI-3 — The `patch_size` class epic

**Objective:** `habitat_connectivity()` returns a `patch_size` object carrying
metadata; downstream functions dispatch on it. Built on the final (WI-2) vocabulary.

**Relates to existing issues:**
- **#133** — "Add a `patch_area` class to `habitat_connectivity()`." This epic *is* #133;
  we land it as `patch_size` (name rationale below) rather than `patch_area`.
- **#10** — "Consider S3/S7 methods so we can have the same function names for raster or
  vector approaches." Our S3 class is the foundation for that; record the
  S3-tibble-subclass decision against #10.
- **#35** — "Implement Design scenario testing." The class + `compare_connectivity()`
  consuming two `patch_size`s is the scenario-testing workflow.
- **#13** — "allow for use of original environment area" in LOO summarising. This is the
  `area_baseline` concept; the class should make passing a baseline natural.

### 3a. `patch_size` class + metadata on `habitat_connectivity()` — S3 tibble subclass

> **Name:** the class is `patch_size`, **not** `patch_area`, to avoid confusion
> with the existing `add_patch_area()` / `sf_add_patch_area()` pipeline functions (the
> constructor would otherwise sit right next to them). It carries the "connectivity"
> signal of `habitat_connectivity()` and reads naturally on the result.

- `habitat_connectivity()` gains a `species` (a.k.a. `label`) arg and returns a
  `patch_size` carrying `species` and `interpatch_distance` as attributes.
- **Do NOT store `area_squared`** (derived — the point of #128). Keep it inside
  `effective_mesh_ha()`.
- **Structure: S3 tibble subclass** via a validating constructor. The object *is* the
  tibble (class `c("patch_size", "tbl_df", "tbl", "data.frame")`); metadata rides
  along as attributes. This gives `$`, `[`, `DT::datatable()`, `write.csv()` and ggplot2
  for free — no need to re-implement data-frame ergonomics.
- **Why subclass, not S7 composition:** the result is *terminal and read-only* (built once
  at the end of the sf/terra pipelines, then only read/exported), so dplyr stripping the
  class/attributes on `mutate()`/`filter()` never bites in practice — and dplyr strips S7
  attributes too, so that's a wash, not a reason to prefer S7. No double dispatch or
  compute-on-demand is needed, so S7's machinery earns nothing here; validation (the one
  real win) comes from the constructor.
- **Sketch (as implemented in `R/connected-patches.R`):**
  ```r
  patch_size <- function(data, species, interpatch_distance) {
    if (!rlang::is_scalar_character(species)) {
      cli::cli_abort("{.arg species} must be a character vector of length 1.")
    }
    if (!is.numeric(interpatch_distance) || length(interpatch_distance) != 1) {
      cli::cli_abort("{.arg interpatch_distance} must be a numeric vector of length 1.")
    }
    if (!"area" %in% names(data)) {
      cli::cli_abort("{.arg data} must contain an {.field area} column.")
    }
    tibble::new_tibble(
      tibble::as_tibble(data),
      species = species,
      interpatch_distance = interpatch_distance,
      nrow = nrow(data),
      class = "patch_size"
    )
  }
  ```
- **Metadata accessors:** `pc_species(x)` and `pc_interpatch_distance(x)` (thin wrappers
  over `attr()`), so downstream reads metadata without poking at attributes directly.
- Add a `print.patch_size` method: one-line metadata header (`cli`) + `NextMethod()`
  to fall through to the normal tibble print.
- **Package setup:** `tibble`/`cli`/`rlang` are already in DESCRIPTION Imports. **No S7
  dependency, no `R/zzz.R`, no `methods_register()`** — the `print` method is registered as
  an ordinary `S3method()` (roxygen `@export`).

### 3b. Resolution-metadata decision (folds in former "Step 3")

- `target_resolution` / `data_resolution` / `aggregation_factor` are **never used in a
  metric formula** (only `area` is) — pure provenance.
- **Recommendation:** attach as named metadata on the object (set upstream where the
  raster is actually aggregated, inside `habitat_connectivity()`), not as function args.
  Prefer a few named provenance fields over a generic `.extra_cols = list()` bag. Confirm
  via grep that nothing reads them before relocating.
- Could be split into its own small issue if preferred; otherwise decide it here.

### 3c. Class-aware generics

- `summarise_connectivity` and `compare_connectivity` become **S3 generics** (`UseMethod()`),
  with a `patch_size` method and a numeric default (the power-user vector entry
  point). No S7 generics.
- **Sketch:**
  ```r
  summarise_connectivity <- function(area, ...) UseMethod("summarise_connectivity")

  summarise_connectivity.patch_size <- function(area, ...) {
    connectivity_metrics(area$area)   # + bind metadata off pc_species()/pc_interpatch_distance()
  }
  summarise_connectivity.default <- function(area, area_baseline = area, ...) {
    connectivity_metrics(area, area_baseline)   # numeric vector entry point
  }

  compare_connectivity <- function(area_new, ...) UseMethod("compare_connectivity")

  compare_connectivity.patch_size <- function(area_new, area_baseline, ...) {
    if (!identical(pc_species(area_new), pc_species(area_baseline))) {
      cli::cli_abort("Scenarios must be the same species.")
    }
    # summarise each, diff
  }
  ```
- Single dispatch on the first argument (homogeneous pair); double dispatch not needed.
  Because `patch_size` *is* a data frame, a numeric column can still be pulled with
  `area$area` and routed to the default method when a raw vector is passed.
- **Validation rule:** `species` must match across scenarios; `interpatch_distance` and the
  habitat/barrier scenario are *allowed* to differ (comparing across buffers/scenarios is
  legitimate).

**Verification (epic):** the worked comparison runs end to end with `species` supplied
once at `habitat_connectivity()`; a mismatched-species comparison is rejected.

---

