#include<iostream>
#include<vector>
#include<string>
#include<cmath>
#include<algorithm>
#include<fstream>
#include<sstream>
#include<utility>
using namespace std;

// Set-Up 
struct MarketQuote{
    string Days;
    int MaturityDays;
    double CashRate; 
    double ParSwapRate; 
};
struct NewSwapInfo{
    double FixedRate;
    int MaturityDays;
    int FixedLegFreqDays;
    int FloatLegFreqDays;
};
const double YearDays=360.0;
double DiscountCF(int t1,int t2){
    return static_cast<double>(t2-t1)/YearDays;
}
int DayConversion(const string&days){
    if(days.empty()){
        return 0;
    }
    char unit=days.back();
    string numP=days.substr(0,days.size()-1);
    int value=0;
    try{
        value=stoi(numP); 
    }
    catch(...){
        return 0;
    }
    if(unit=='D'){
        return value;
    }    
    else if(unit=='W'){
        return value*7;
    }  
    else if(unit=='M'){
        return value*30;
    } 
    else if(unit=='Y'){
        return value*360;
    } 
    return 0;    
}
int FreqConversion(const string&freq){
    if(freq=="1m"){
        return 30;
    }
    if(freq=="3m"){
        return 90;
    }   
    if(freq=="6m"){
        return 180;
    } 
    if(freq=="12m"){
        return 360;
    }
    return 180;
}

class Interpolator{
public:
    virtual double interpolate(double targetX,const vector<double>&x,const vector<double>&y)const=0;
    virtual ~Interpolator()=default;
};

class LinearInterpolator:public Interpolator{
public:
    double interpolate(double tX,const vector<double>&x,const vector<double>&y)const override{
        if(x.empty()||x.size()!=y.size()){
            return 0.0;
        } 
        if(tX<=x.front()){
            return y.front();
        }
        if(tX>=x.back()){
            return y.back();
        }
        for (size_t i=0;i<x.size()-1;i++){
            if(tX>=x[i]&&tX<=x[i+1]){
                if(tX==x[i]){
                    return y[i];
                }
                if(tX==x[i+1]){
                    return y[i+1];
                }
                double weight=(tX-x[i])/(x[i+1]-x[i]);
                return y[i]+weight*(y[i+1]-y[i]);
            }
        }
        return 0.0;
    }
};

class QuadraticInterpolator:public Interpolator{
private:
    double Parabola(double target,double x0,double y0,double x1,double y1,double x2,double y2)const{
        double L0=((target-x1)*(target-x2))/((x0-x1)*(x0-x2));
        double L1=((target-x0)*(target-x2))/((x1-x0)*(x1-x2));
        double L2=((target-x0)*(target-x1))/((x2-x0)*(x2-x1));
        return y0*L0+y1*L1+y2*L2;
    }

public:
    double interpolate(double tX,const vector<double>&x,const vector<double>&y)const override{
        int n=x.size();
        if(n<3){
            LinearInterpolator lin;
            return lin.interpolate(tX,x,y);
        }
        if(tX<=x.front()){
            return y.front();
        }
        if(tX>=x.back()){
            return y.back();
        }
        for(int i=0;i<n-1;i++){
            if(tX>=x[i]&&tX<=x[i+1]){
                if(tX==x[i]){
                    return y[i];
                }
                if(tX==x[i+1]){
                    return y[i+1];
                }
                bool hasL=(i>0);
                bool hasR=(i+2<n);

                double valL=0.0,valR=0.0;
                
                if(hasL){
                    valL=Parabola(tX,x[i-1],y[i-1],x[i],y[i],x[i+1],y[i+1]);
                }
                if(hasR){
                    valR=Parabola(tX,x[i],y[i],x[i+1],y[i+1],x[i+2],y[i+2]);
                }
                if(hasL&&hasR){
                    return (valL+valR)/2.0;
                }
                if(hasL){
                    return valL;
                }
                return valR;
            }
        }
        return 0;
    }
};

class DiscountCurve{
private:
    vector<double>Maturities;
    vector<double>DiscountF;
    Interpolator*interpolator;
public:
    DiscountCurve(Interpolator* Interp):interpolator(Interp){}
    
    void Cali(const vector<MarketQuote>&MarketData,int type){
    vector<pair<double,double>>Points;
    Points.push_back({0.0, 1.0});
    for(const auto&q:MarketData){
        double Rate=(type==0)?(q.CashRate/100.0):(q.ParSwapRate/100.0);
        if(Rate==0.0){
            continue;
        }
        double t=q.MaturityDays/360.0;
        double DF=1.0/(1.0+(Rate*t));
        Points.push_back({static_cast<double>(q.MaturityDays),DF});
    }
    sort(Points.begin(),Points.end());
    Maturities.clear();
    DiscountF.clear();
    for(const auto&p:Points){
        Maturities.push_back(p.first);
        DiscountF.push_back(p.second);
    }
}
    double getDF(double tDay)const{
        return interpolator->interpolate(tDay,Maturities,DiscountF);
    }
};

double CalPV(const NewSwapInfo&Swap,const DiscountCurve&Curve) {
    double N=100.0;
    double FixedPV=0.0;
    double FloatPV=0.0;
    // Pays This
    double FixedD=Swap.FixedLegFreqDays/360.0;
    for (int t=Swap.FixedLegFreqDays;t<=Swap.MaturityDays;t+=Swap.FixedLegFreqDays){
        FixedPV+=N*(Swap.FixedRate/100.0)*FixedD*Curve.getDF(t);
    }
    // Receives This
    double FloatD=Swap.FloatLegFreqDays/360.0;
    double PrevDF=1.0;
    for (int t=Swap.FloatLegFreqDays;t<=Swap.MaturityDays;t+=Swap.FloatLegFreqDays){
        double CurrentDF=Curve.getDF(t);
        double ForwardR=(PrevDF/CurrentDF-1.0)/FloatD;
        FloatPV+=N*ForwardR *FloatD*CurrentDF;
        PrevDF=CurrentDF;
    }
    return FloatPV-FixedPV; 
}

double CalPSR(const NewSwapInfo&Swap,const DiscountCurve&Curve) {
    double N=100.0;
    double FloatPV=0.0;
    double Annuity=0.0;
    double FloatD=Swap.FloatLegFreqDays/360.0;
    double PrevDF=1.0;
    for (int t=Swap.FloatLegFreqDays;t<=Swap.MaturityDays;t+=Swap.FloatLegFreqDays){
        double CurrentDF=Curve.getDF(t);
        double Forward=(PrevDF/CurrentDF-1.0)/FloatD;
        FloatPV+=N*Forward*FloatD*CurrentDF;
        PrevDF=CurrentDF;
    }
    double FixedD=Swap.FixedLegFreqDays/360.0;
    for (int t=Swap.FixedLegFreqDays;t<=Swap.MaturityDays;t+=Swap.FixedLegFreqDays){
        Annuity+=N*FixedD*Curve.getDF(t);
    }
    return (FloatPV/Annuity)*100.0;
}

vector<double>CalSens(const vector<MarketQuote>&MarketData,const NewSwapInfo&Swap,Interpolator* interp,int type){
    vector<double>Sensitivities(MarketData.size(),0.0);
    vector<double>X;
    vector<double>BaseDFs;
    X.push_back(0.0);
    BaseDFs.push_back(1.0);
    for(size_t i=0;i<MarketData.size();i++){
        double Rate=(type==0)?MarketData[i].CashRate:MarketData[i].ParSwapRate;
        double ty=MarketData[i].MaturityDays/360.0;
        double df=1.0/(1.0+(Rate/100.0)*ty);
        X.push_back((double)MarketData[i].MaturityDays);
        BaseDFs.push_back(df);
    }
    
    double N=100.0;
    double FixedD=Swap.FixedLegFreqDays/360.0;
    for(size_t i=0;i<MarketData.size();i++){
        double Rate=(type==0)?MarketData[i].CashRate:MarketData[i].ParSwapRate;
        if(Rate==0.0){
            continue;
        } 
        
        double ty=MarketData[i].MaturityDays/360.0;
        double df_i=BaseDFs[i+1]; 
        double dDF_dRate=-(MarketData[i].MaturityDays/36000.0)*(df_i*df_i);
        
        vector<double>Y_weights(X.size(),0.0);
        Y_weights[i+1]=1.0; 
        double dPV_dDF_i=0.0;
        for(int t=Swap.FixedLegFreqDays;t<=Swap.MaturityDays;t+=Swap.FixedLegFreqDays){
            double Weight=interp->interpolate(t,X,Y_weights);
            double dPV_dDF_t=-N*(Swap.FixedRate/100.0)*FixedD;
            dPV_dDF_i+=dPV_dDF_t*Weight;
        }
        double Weight_m=interp->interpolate(Swap.MaturityDays,X,Y_weights);
        dPV_dDF_i+=(-N)*Weight_m;
        Sensitivities[i]=dPV_dDF_i*dDF_dRate;
     }
     return Sensitivities;
}

int main(){
    ifstream file("Input.csv");
    if(!file.is_open()){
        return -1;
    }
    string line;
    vector<MarketQuote>MarketData;

    getline(file,line);
    line.erase(remove(line.begin(), line.end(),'\r'),line.end());
    stringstream ssN(line);
    string nStr;
    getline(ssN,nStr,',');
    string cleanN="";
    for(char c:nStr){
        if(isdigit(c)){
            cleanN+=c;
        }
    }
    int N=0;
    try{
        N=stoi(cleanN);
    }
    catch(...){ 
        return 1; 
    }
    for (int i=0;i<N;i++){
        if(!getline(file,line)){
            break;
        }
        line.erase(remove(line.begin(), line.end(),'\r'),line.end());
        stringstream ss(line);
        string daysStr,cashStr,swapStr;
        getline(ss,daysStr,',');
        getline(ss,cashStr,',');
        getline(ss,swapStr,',');

        MarketQuote q;
        q.Days=daysStr;
        q.MaturityDays=DayConversion(daysStr);
        try{
            q.CashRate=stod(cashStr);
        } 
        catch(...){
            q.CashRate=0.0;
        }
        try{ 
            q.ParSwapRate=stod(swapStr);
        } 
        catch(...){ 
            q.ParSwapRate=0.0;
        }
        MarketData.push_back(q);
    }

    double q1Time=0.0;
    if(getline(file,line)){
        line.erase(remove(line.begin(),line.end(),'\r'),line.end());
        stringstream ssQ(line);
        string q1Str;
        getline(ssQ,q1Str,',');
        try{ 
            q1Time=stod(q1Str); 
        } 
        catch(...){ 
            q1Time=0.0; 
        }
    }

    NewSwapInfo newSwap;
    string maturityStr="";
    if (getline(file,line)){
        line.erase(remove(line.begin(), line.end(), '\r'), line.end());
        stringstream ssS(line);
        string fixedStr, f1Str, f2Str;
        getline(ssS,fixedStr,',');
        getline(ssS,maturityStr,',');
        getline(ssS,f1Str,',');
        getline(ssS,f2Str,',');

        try{
            newSwap.FixedRate=stod(fixedStr);
        } 
        catch(...){ 
            newSwap.FixedRate=0.0;
        }
        newSwap.MaturityDays=DayConversion(maturityStr);
        newSwap.FixedLegFreqDays=FreqConversion(f1Str);
        newSwap.FloatLegFreqDays=FreqConversion(f2Str);
    }
    file.close();

    LinearInterpolator Linear;
    QuadraticInterpolator Quadratic;

    DiscountCurve cashLin(&Linear),cashQuad(&Quadratic);
    DiscountCurve swapLin(&Linear),swapQuad(&Quadratic);
    
    cashLin.Cali(MarketData,0);
    cashQuad.Cali(MarketData,0);
    swapLin.Cali(MarketData,1);
    swapQuad.Cali(MarketData,1);

    double q1_a=cashLin.getDF(q1Time);
    double q1_b=cashQuad.getDF(q1Time);
    double q1_c=swapLin.getDF(q1Time);
    double q1_d=swapQuad.getDF(q1Time);

    double pv_a=CalPV(newSwap,cashLin),psr_a=CalPSR(newSwap,cashLin);
    double pv_b=CalPV(newSwap,cashQuad),psr_b=CalPSR(newSwap,cashQuad);
    double pv_c=CalPV(newSwap,swapLin),psr_c=CalPSR(newSwap,swapLin);
    double pv_d=CalPV(newSwap,swapQuad),psr_d=CalPSR(newSwap,swapQuad);

    vector<double>sens_a=CalSens(MarketData,newSwap,&Linear,0);
    vector<double>sens_b=CalSens(MarketData,newSwap,&Quadratic,0);
    vector<double>sens_c=CalSens(MarketData,newSwap,&Linear,1);
    vector<double>sens_d=CalSens(MarketData,newSwap,&Quadratic,1);

    ofstream out("Output.csv");
    if(!out.is_open()){
        return 1;
    }

    out<<q1_a<<","<<q1_b<<","<<q1_c<<","<<q1_d<<"\n";
    out<<pv_a<<","<<pv_b<<","<<pv_c<<","<<pv_d<<"\n";
    out<<psr_a<<","<<psr_b<<","<<psr_c<<","<<psr_d<<"\n";
    for (size_t i=0;i<MarketData.size();i++){
        out<<sens_a[i]<<","<<sens_b[i]<<","<<sens_c[i]<<","<<sens_d[i]<<"\n";
    }
    out.close();
    cout<<"Question 2 Complete!\n";
    return 0;
}