# Connectivity API — plan of attack

## Context

The connectivity functions (`habitat_connectivity()`, `summarise_connectivity()`,
`compare_connectivity()`, and the metric primitives in `R/calculations.R`) carry two
kinds of friction:

1. **Repetition / metadata burden** — callers re-supply `species`, the threshold
   distance, and three resolution settings at every step, even when a function never uses
   or displays them.
2. **Naming that describes proximity, not the concept** — the inter-patch connectivity
   threshold is called `distance` (recently unified from an earlier `buffer_distance` /
   `distance` split). It's now consistent, but still named for the raw spatial quantity
   rather than the *concept* the ecologist specifies: the threshold distance for
   inter-patch connectivity.

The end-state: a `patch_size` class produced by `habitat_connectivity()` that carries its
metadata, so downstream functions read it off the object instead of demanding it as args
— built on a consistent, concept-driven vocabulary.

**Working mode:** Claude provides plans, specs, signatures, and decisions; the user writes
the implementation. (Ship's Computer, not Data.)

---

## Glossary — canonical names

Settle these once and use them everywhere (arg, `@property`, data.frame column, docs).
**Code names target the ecologist/R-package audience.** The shiny app's less-expert
audience is served by *UI labels* (free text in the UI layer, e.g. "Maximum distance
between patches (m)") — those are independent of these code names, so don't compromise the
code vocabulary for the app.

| Concept | Current name(s) | Canonical name | Notes |
|---|---|---|---|
| Inter-patch connectivity threshold distance | `distance` (terra), `interpatch_distance` (sf) | **`interpatch_distance`** | The distance between patches below which they count as connected. sf's `sf_habitat_buffer()` already uses this name — only terra's `distance` needs to move. Matches the literature ("threshold distance for inter-patch connectivity"). **WI-2.** |
| Species label | `species` (already at target; was `species_name`) | **`species`** | Shorter, prints cleaner; value is self-evidently a name. **WI-2 species work is effectively done** — except the lagging `generate-report.R`. |
| Effective mesh size (hectares) | `effective_mesh_size()` (fn), `effective_mesh_ha` (col) | **`effective_mesh_ha`** (fn + col) | Per issue #129 — units in the name; column already uses this. |
| Probability of connectedness | `prob_connectedness` | **`prob_connectedness`** (keep) | Clear and reasonable length. |
| Number of patches | `n_patches` | **`n_patches`** (keep) | Fine. |
| Mean patch area | `patch_area_mean` | **`patch_area_mean`** (keep) | Fine. |
| Total patch area (ha) | `patch_area_total_ha` | **`patch_area_total_ha`** (keep) | Fine. |
| Resolution settings | `target_resolution`, `data_resolution`, `aggregation_factor` | **pending** | Demoted to metadata (or dropped) in the class work — see WI-3. If retained as metadata, names can stay. |
| Connected-patches object (new class) | — | **`patch_size`** | An S3 tibble subclass (see #133/WI-3). Named to avoid clashing with `add_patch_area()`; carries the connectivity signal. |

**Function names:**
- `effective_mesh_size()` → **`effective_mesh_ha()`** (issue #129).
- `habitat_connectivity()` → **keep** (descriptive, in use; just return a `patch_size`).
  Considered `patch_areas()`; rejected to avoid churn and to keep the connectivity signal.
  Avoid bare `connectivity()`.
- `summarise_connectivity()` → **keep** the explicit verb. *Recommend against* overloading
  base `summary()` (it implies a cheap human-readable digest; this returns a structured
  tibble). Optionally add a light `summary.patch_size()` for a console digest later.

---

## Order of operations (revised)

The naming redux now comes **before** the class work: the moment the class ships, names
freeze into the *public* surface (`x@species`, columns, user scripts), so renaming later
is real breakage. Rename while it's still cheap.

```
DONE   #128  area_squared removed (merged via #130)

WI-1   #124  compare_connectivity() simplification         ── in flight, small
                │ (uses effective_mesh_ha already; metric
                │  column names unaffected by the redux)
                ▼
WI-2   #131 (closes #129)  Naming redux                     ── mechanical, low-risk
                │ interpatch_distance, species, effective_mesh_ha()
                ▼
WI-4   #131 (folded in)  interpatch-distance semantics      ── breaking + version bump
                │ habitat_buffer(buffer_radius); concept fns take
                │ interpatch_distance XOR buffer_radius
                ▼
WI-3   #133 (refs #10, #35, #13)  patch_size class epic ── built on final names + signatures
                  - patch_size S3 (tibble subclass) class + metadata
                  - resolution-metadata decision
                  - class-aware summarise_connectivity() / compare_connectivity()
```

Why WI-1 stays first: it's the in-flight branch and is largely insulated from the redux
(its minimal output is `scenario`/`n_patches`/`effective_mesh_ha`/`prob_connectedness` —
no `distance`/`species`, and `effective_mesh_ha` is already the #129 target). The only
redux touch-up later is trivial.

---

## Work items

**Done / merged** (planning docs removed):
- ~~WI-1 — `compare_connectivity()` simplification~~ — **#124 closed**.
- ~~WI-2 — naming redux~~ (`interpatch_distance`, `species`, `effective_mesh_ha()`) — **#131 / #129 closed**.
- ~~WI-4 — interpatch-distance semantics~~ (full distance vs buffer radius; breaking) — **folded into #131, closed**.

**Outstanding:**
- **[WI-3 — `patch_size` class epic](wi-3-connected-patches-class.md)** — S3 tibble-subclass class + class-aware generics (#133; refs #10, #35, #13 — all open).

Supporting material:
- The resolution mechanism (sub-cell buffers) is documented in
  `vignette("interpatch-distance-and-resolution")`.

---

## Issue mapping

| Work item | Existing issue? | Action |
|---|---|---|
| WI-1 compare_connectivity simplification | **#124** (exists) | Implement on the current branch; PR closes #124. |
| WI-2 naming redux | partial — **#129** exists (effective_mesh) | Open a **new** "naming redux" issue covering `interpatch_distance` + `species` + function names; PR **closes #129** too. |
| WI-3a/3c patch_size class + dispatch | **#133** (exists); **refs #10** | Deliver #133 as `patch_size`; cross-reference #10 (S3/S7), and note it lays groundwork for #10's raster/vector goal. |
| WI-3 scenario-testing workflow | **#35** (exists) | The class + compare workflow delivers #35; reference it from the WI-3 issue / close when delivered. |
| WI-3 baseline area in LOO | **#13** (exists) | `area_baseline` support relates to #13; cross-reference. |
| WI-3b resolution metadata | none | Decide within WI-3, or split into a **new** small issue if it grows. |
| WI-4 interpatch-distance semantics | **folded into #131/#129** (this branch) | Resolve on the current naming branch; PR scope = naming + semantics (breaking) + version bump. |
| habitat_connectivity changes | **#108** (exists, related) | Cross-reference if `habitat_connectivity_full()` consolidation overlaps. |

**New issues to write:** none outstanding — the class epic is tracked as **#133**. WI-4 is
folded into the naming branch (#131/#129); everything else maps onto existing issues
(#124, #10, #35, #13, #108).

---

## Plan of attack (summary)

1. **WI-1 / #124 now.** Smallest, in flight, independent. Land the `connectivity_metrics()`
   helper + minimal `compare_connectivity()` + `summarise_connectivity()` refactor. PR
   closes #124.
2. **Write the naming-redux issue** (the class epic is already tracked as #133) so the
   sequence is tracked, cross-referencing #129, #133, #10, #35, #13, #108.
3. **WI-2 naming redux next.** Settle the glossary, then one mechanical rename PR
   (`interpatch_distance`, `species`, `effective_mesh_ha()`); closes #129. Isolated diff.
4. **WI-4 interpatch-distance semantics — same branch as WI-2.** Breaking change folded
   into the naming branch: `habitat_buffer()` takes `buffer_radius`, `habitat_connectivity*()`
   take `interpatch_distance` XOR `buffer_radius` via `resolve_buffer_radius()`, plus
   `warn_buffer_resolution()`. Add the version bump, `NEWS`, and doc/vignette/example/shiny
   sweep. Lands **before** WI-3 so the class is built on the final signatures.
5. **WI-3 class epic last (#133).** Decide the resolution-metadata home, build the
   `patch_size` S3 (tibble subclass) class, then make `summarise_connectivity()` /
   `compare_connectivity()` class-aware. Delivers #133 and #35; advances #10; relates to #13.

**Sequencing rule that drives all of this:** names *and* signatures must be final (WI-2,
then WI-4) before they get frozen into the public class surface (WI-3). WI-1 is safe to do
first because it doesn't touch the renamed vocabulary or the distance semantics.
