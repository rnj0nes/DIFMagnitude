# Future Tasks

## 2. Add a Runnable End-to-End Example Script

- Create an examples script that demonstrates loading inputs, extracting parameters, computing effect sizes, and plotting outputs with reproducible sample data.
- Keep example dependencies explicit (including mplush5/rhdf5 expectations).
- Confirm the script runs start-to-finish in a fresh session.

## 3. Add Initial testthat Coverage

- Add testthat scaffolding and first tests for compute_areas and pooled_sd.
- Cover nominal behavior and at least one edge case for each function.
- Ensure tests run cleanly under devtools::test() and devtools::check().
