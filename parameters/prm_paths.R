# path
{
  path_packages<<-file.path(path_folder, "packages")
  path_parameters<<-file.path(path_folder, "parameters")
  path_documentation<<-file.path(path_folder, "documentation")
  path_sources<<-file.path(path_folder, "sources")
  path_app<<-file.path(path_folder, "app")
  
  path_modules<<-file.path(path_app, "modules")
    path_functions<<-file.path(path_modules, "functions")
  
  path_data<<-file.path(path_app, "data")
    path_input<<-file.path(path_data, "input")
    path_input_tqGet<<-file.path(path_input, "tqGet")
    path_input_forexSb<<-file.path(path_input, "forexsb")
    path_output<<-file.path(path_data, "output")
    path_output_metrics<<-file.path(path_output, "metrics")
    path_output_signals<<-file.path(path_output, "signals")
    path_output_forecast<<-file.path(path_output, "forecast")
    path_output_graphs<<-file.path(path_output, "graphs")
    path_output_tables<<-file.path(path_output, "tables")
}
