## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

## Response to Uwe Ligges’ Comments

1.	Possible Typo in the DESCRIPTION
  *	The term httr has been corrected to httr2.

2. Links in the Title
	* It has been updated to remove any links.

Thank you for your feedback!

## Response to Konstanze Lauseker


1. Please always write package names, software names and API (application
    programming interface) names in single quotes in title and description.
    e.g: --> 'Diario'
    Please note that package names are case sensitive.
  * The Description has been updated to comply.
  
2. Please unwrap the examples if that is feasible and if they can be
    executed in < 5 sec for each Rd file or create additionally small toy
    examples to allow automatic testing.
  * We removed \donttest{} and \dontrun{} from diario_store_token and diario_retrieve_token. 	For the other functions, because they depend on valid credentials and/or 
	  external services, we kept their examples in \donttest{} or \dontrun{} to 
	  avoid potential runtime or dependency issues on CRAN's test systems.

