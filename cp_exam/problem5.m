steps=100;bins=1000;
x(bins)=0;
y(bins)=0;
for i=1:1:bins
    x(i)=length(find(unifrnd(0,1,steps,1)<=0.5));
    y(i)=length(find(unifrnd(0,1,steps,1)<=0.5));
end
ave_x=sum(x)/bins;ave_y=sum(y)/bins;
asxy=sum(x.^2+y.^2)/bins;
% <x>=50.2026 ; <y>=50.1740 ; <x^2+y^2>=5090.6