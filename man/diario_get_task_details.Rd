% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diario.R
\name{diario_get_task_details}
\alias{diario_get_task_details}
\title{Get details of a specific task}
\usage{
diario_get_task_details(project_id, task_id)
}
\arguments{
\item{project_id}{A valid non-empty string with the project ID.}

\item{task_id}{A valid non-empty string with the task ID.}
}
\value{
A list containing task details.
}
\description{
This function retrieves details of a specific task by task ID within a project.
}
\examples{
\dontrun{
task <- diario_get_task_details("66cf438223aa80386306e647", "66cf44209e4fedefb306bcd3")
}
}
