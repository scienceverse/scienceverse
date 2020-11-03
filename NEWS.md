# scienceverse 0.0.0.9018 (2020-11-03)

* fixed bug in `study_power()` that produced errors for long (as opposed to wide) data with within-subject factors
* changed the returned llist from `get_power()` to return all analysis results, even those that aren't used in hypothesis criteria
* as always, more app updates

# scienceverse 0.0.0.9017 (2020-10-28)

* `scivrs_app()` updates
* some new helper functions (behind the scenes)
* `add_hypothesis()` now takes custom arguments
* `study_demo` and `kin` data 

# scienceverse 0.0.0.9016 (2020-10-17)

* Still working on `scivrs_app()`

# scienceverse 0.0.0.9015 (2020-10-03)

* Lots of changes to `scivrs_app()` 
    - It's still not-production ready
    - But check out the demo

# scienceverse 0.0.0.9014 (2020-09-30)

* `get_orcid()` lets you look up ORCiDs from family and given name
* New vignette about CRediT authorship format and ORCiDs

# scienceverse 0.0.0.9013 (2020-08-14)

* Updated for faux 0.0.1.4 (no seed argument)
* New vignette

# scienceverse 0.0.0.9012 (2020-07-21)

* Shiny App!! Start with `scivrs_app()`
* `update_hypothesis()`, `update_analysis()` and `update_criterion()` functions
* codebook updated to most recent Psych-DS spec (still experimental)
* various bug fixes

# scienceverse 0.0.0.9011 (2020-07-19)

* `make_script()`, `make_data()` functions
* new capabilities for `get_results()` function
* Vignette on exporting study materials from the meta-study file.

# scienceverse 0.0.0.9010 (2020-07-08)

* Better `output_****` functions
* Each study object now loads its data and functions in its own environment to avoid namaspace clashes
* Various bug fixes to prevent crashes
* Demo app (in progress) with `scivrs_app()`

# scienceverse 0.0.0.9009 (2020-06-25)

* Fixes for codebook changes in faux

# scienceverse 0.0.0.9008 (2020-06-23)

* New codebook vignette and enhanced codebook functions from faux

# scienceverse 0.0.0.9007 (2020-03-17)

* Streamlined how code is stored in the JSON file
* added `get_result` function

# scienceverse 0.0.0.9006 (2020-03-14)

* More breaking changes to update the format for the preprint
* Loading study objects from JSON works now
* `study_report` deprecated and function merged to `study_save`
* added `get_data` function

# scienceverse 0.0.0.9004 (2020-03-12)

* Added the `study_power` function. This function is experimental. Check power analyses with an external package before using for important decisions. 

# scienceverse 0.0.0.9003 (2020-03-10)

* Added a `NEWS.md` file to track changes to the package.
* Lots of breaking changes to align the format with the preprint  
    Lakens, D., & DeBruine, L. M. (2020, January 27). Improving Transparency, Falsifiability, and Rigour by Making Hypothesis Tests Machine Readable. <https://doi.org/10.31234/osf.io/5xcda>
