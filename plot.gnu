set term png
set output "TN_plot.png"
set pm3d map
splot "-" using 1:2:3 with pm3d title "TN"
