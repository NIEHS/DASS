for /f "delims=" %%i in ('where Rscript.exe') do set rloc="%%i"
%rloc% ".\launch_shiny.R"