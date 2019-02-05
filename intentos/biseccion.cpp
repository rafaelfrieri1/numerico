

#include <iostream>
 using namespace std;
int func(float x)
{
    return ((x*2)-2);
}

float bis(float a,float b)
{
    if (a > b )
    {
        return(bis(a,b));
    }
    float c = (a+b)/2;
    if(abs(func(c)) <= 0.1){
        return(c);
    }
    if(func(a)*func(c)<0){
        return (bis(c,b));
    }

    return 0;
}

int main()
{
    cout<<bis(0,4)<<endl;

    return 0;
}




