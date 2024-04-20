#include <iostream>
#include <array>

using namespace std;

struct room
{
    const int beds = 2;
};

struct hotel
{
    room* open[10] = {NULL};
    room* occupied[10] = {NULL};
    room* toClean[10] = {NULL};
};

hotel clean(hotel hilton);
hotel checkIn(hotel hilton);
hotel checkOut(hotel hilton);

int main() 
{
    hotel hilton;
    for(int i=0; i<10; i++)
    {
        room* rm = new room();
        hilton.open[i] = rm;
    }

    int action;
    bool on = true;

    while(on)
    {
        cout<<"Input action:\n\t1 to check in\n\t2 to check out\n\t3 to clean a room\n\t4 to end program\nInput: ";
        cin>>action;
        
        switch(action)
        {
            case 1:
                hilton = checkIn(hilton);
                break;
            case 2:
                hilton = checkOut(hilton);
                break;
            case 3:
                hilton = clean(hilton);
                break;
            case 4:
                cout<<"Exiting..."<<endl;
                on = false;
                break;
            default:
                cout<<"Invalid input"<<endl;
        }
    }
    return 0;
}

hotel checkIn(hotel hilton)
{
    if(hilton.open[0] != NULL)
    {
        hotel h = hilton;
        for(int i=0; i<10; i++)
        {
            if(h.occupied[i] == NULL)
            {
                h.occupied[i] = h.open[0];
                break;
            }
        }
        for(int i=0; i<9; i++)
        {
            h.open[i] = h.open[i+1];
            if(h.open[i] == NULL || i==9)
            {
                h.open[i] = NULL;
                break;
            }
        } 
        cout<<"Got you checked in."<<endl;
        return h;
    }
    else
    {
        cout<<"No rooms avalible."<<endl;
        return hilton;
    }
}

hotel checkOut(hotel hilton)
{
    if(hilton.occupied[0] != NULL)
    {
        hotel h = hilton;
        for(int i=0; i<10; i++)
        {
            if(h.toClean[i] == NULL)
            {
                h.toClean[i] = h.occupied[0];
                break;
            }
        }
        for(int i=0; i<9; i++)
        {
            h.occupied[i] = h.occupied[i+1];
            if(h.occupied[i] == NULL || i==9)
            {
                h.occupied[i] = NULL;
                break;
            }
        } 
        cout<<"Got you checked out."<<endl;
        return h;
    }
    else
    {
        cout<<"No one to check out."<<endl;
        return hilton;
    }
}

hotel clean(hotel hilton)
{
    if(hilton.toClean[0] != NULL)
    {
        hotel h = hilton;
        for(int i=0; i<10; i++)
        {
            if(h.open[i] == NULL)
            {
                h.open[i] = h.toClean[0];
                break;
            }
        }
        for(int i=0; i<9; i++)
        {
            h.toClean[i] = h.toClean[i+1];
            if(h.toClean[i] == NULL || i==9)
            {
                h.toClean[i] = NULL;
                break;
            }
        } 
        cout<<"Cleaned room."<<endl;
        return h;
    }
    else
    {
        cout<<"No rooms to clean."<<endl;
        return hilton;
    }
}



