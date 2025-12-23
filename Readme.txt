Readme

I will not be around to maintain any of this beyond this point. So feel free to do whatever you like with it.



The goal of this script to standardize an ASD related subset of features across multiple years from census NSCH data. 

There are a few steps to getting the data cleaned up:

1: Run label-define-csv-generator.R
	You only need to do this once if you are starting from scratch, otherwise just use the files I have in the directory
	This script generates the csv define files from the source .do stata files.
	In all the years I worked with I had to manually fix the values for field k2q01_d, something was missing in the .do files or I misunderstood the variable in some way


2: Run data-cleanup.R

	This is the main script it generates a final .rds file for the data.
	Unless something substantial about future NSCH datasets changes this should work on stata format data downloads for years 2016-2023
	You should only need to adjust stuff in the variable-config.json file
	I tried to build it to be controlled off just that file.

	If you want to add a new feature just add it to:
	
	"desired_variables": [
 
	but be sure to check that any variables you add are identical across all the years you plan to use, oh and double check the actual .do files as well if something gets weird, sometimes the code labels they use in the online codebook aren't 100% accurate to what is actually in the data, its times like those I missed fry corner the most.
	
	If the variable you need to add has issues, then you just need to make an adjustment. I tried to hack together a system with the json controller to cover all the adjustments you might encounter.

	"transformations": {

	
	inside this there are 3 more sections:

	"transform": {
	"rename_columns": {
	"merge_columns": {

	
	Transform can handle most any issue, just look at one of the others to see what you can do with it:

	"a2_relation": {
		"years": ["2016", "2017", "2018"],
		"value": ["5", "8"],
		"new_value": ["6", "8"],
		"new_label": ["Other: Relative", "There is only one primary adult caregiver in the household for this child"]
		},

	you can set the years the transform will take effect in and you set the old value and the new value, they don't have to be different, and the new labels also doesn't need to be different from the old one, this was my hacky way of making it multipurpose, you can use this to rearrange categories or you can use it to bully the label strings into alignment.

	rename_columns is very simple it's just so you can change the name of columns, this is necessary when they change up the names of features across years.

	Lastly merge_columns:

		"merge_columns": {
			"sleep": {
				"years": ["2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"],
				"column_1": "hoursleep",
				"column_2": "hoursleep05"

	This one is slightly different format, "sleep" is the new name post merge, and then you just enter the years and the two features you want to merge.




3: The last thing you need to do is run test-clean-data.R which outputs a bunch of diagnostics in csv form that you can use to make sure you don't have misaligned features. Most all of this stuff is just making sure the category values (1,2,3) match the category labels ("string label") if they don't then features wind up with NA or the wrong number of categories.
