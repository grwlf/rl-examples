set grid back ls 102
set yrange [-20:20]
set terminal x11 1 noraise
plot "data" using 1:2 with lines,
pause -1

