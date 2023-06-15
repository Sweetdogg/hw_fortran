#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define pi acos(-1)
const double diameter=0.2;
void euler(double *vy,double *vx,double *x,double *y,double *dt,double *t){
	int v=sqrt(pow(*vy,2)+pow(*vx,2));
	*vx += -pow(diameter,3)*v*(*vx)*(*dt);
	*vy += (-pow(diameter,3)*v*(*vy)-9.8)*(*dt);
	*y += (*vy)*(*dt);
	*x += (*vx)*(*dt);
	*t += (*dt);
}
int main(){
       	FILE *fp = NULL;
	fp = fopen("test.txt", "w");
	double theta;
	double v0,x,y,t,dt=0.001;
	double vx,vy;
	for(theta=0;theta<=pi/2;theta+=5*pi/180){
		x=0;y=1.5;t=0;v0=30;
		vx=v0*cos(theta);vy=v0*sin(theta);	
		while(t<100){       
	      		euler(&vy,&vx,&x,&y,&dt,&t);
			if(y<0) break;
		}
	fprintf(fp,"%f %f\n",theta,x);

	}
	
	fclose(fp);
}
