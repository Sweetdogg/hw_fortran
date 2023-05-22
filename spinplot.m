clear;
spin=load('C:\Users\ È°£¿Ì\Desktop/spin.dat');
temp = 0.3000;

[x, y] = meshgrid(1:size(spin, 2), 1:size(spin, 1));
u = cos(spin);
v = sin(spin);
quiver(x, y, v, u, 0.5);
axis([0, size(spin, 2) + 1, 0, size(spin, 1) + 1]);
title(['Temperature = ', num2str(temp)]);
legend('spin')



