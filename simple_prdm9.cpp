#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <map>
#include <random>

using namespace std;

// RANDOM

// stream of pseudo-random bits
static random_device rdev{};
static default_random_engine e{rdev()};


// pop size
int N;
double U;
double rho;
double alpha;
double u;

int Nallele;
map<int, int> allele_count;
map<int, double> allele_activity;


// return uniform number in (0..n-1)
int choose(int n)   {
    uniform_int_distribution<int> d(0,n-1);
    return d(e);
}

// binomial 
int binomial_draw(int n, double p)  {
    binomial_distribution<int> b(n,p);
    return b(e);
}

// multinomial
vector<int> multinomial_draw(int n, const vector<double>& p)    {
    vector<int> ret(n,0);
    discrete_distribution<int> d(p.begin(),p.end());
    for (int i=0; i<n; i++) {
        ret[d(e)]++;
    }
    return ret;
}

void create_allele(int in_count)    {
    allele_count[Nallele] = in_count;
    allele_activity[Nallele] = 1.0;
    Nallele++;
}

void delete_allele(int allele)  {

}

void erode()    {
    for (auto& p : allele_count)    {
        double f = ((double) p.second) / 2 / N;
        allele_activity[p.first] *= exp(-rho*f);
    }
}

void mutate()   {
    for (auto& p : allele_count)    {
        int nmut = binomial_draw(p.second, u);
        for (int i=0; i<nmut; i++)  {
            p.second--;
            create_allele(1);
        }
    }
}

double get_fitness(double activity) {
    return exp(alpha*log(activity));
}

double get_mean_fitness(int allele) {
    double theta1 = allele_activity[allele];
    double mean = 0;
    for (auto& p : allele_count)    {
        double theta2 = allele_activity[p.first];
        double f = ((double) p.first) / 2 / N;
        mean += f * get_fitness(0.5*(theta1 + theta2));
    }
    return mean;
}

void resample_population()  {

}

void cleanup_population()   {
    vector<int> v;
    for (auto& p : allele_count)    {
        if (! p.second) {
            v.push_back(p.first);
        }
    }
    for (auto allele : v)   {
        allele_count.erase(allele);
        allele_activity.erase(allele);
    }
}

// summary statistics 

double get_mean_activity()  {
    double mean = 0;
    for (auto& p : allele_count)    {
        // p.first: allele id
        // p.second: count
        double f = ((double) p.second) / 2 / N;
        double theta = allele_activity[p.first];
        mean += f * theta;
    }
    return mean;
}

double get_diversity()  {
    double m2 = 0;
    for (auto p : allele_count)    {
        double f = ((double) p.second) / 2 / N;
        m2 += f*f;
    }
    return 1.0 / m2;
}


int main(int argc, char* argv[])    {

    N = atoi(argv[1]);
    U = atof(argv[2]);
    rho = atof(argv[3]);
    alpha = atof(argv[4]);
    int every = atoi(argv[5]);
    int until = atoi(argv[6]);

    // initialize
    Nallele = 0;
    create_allele(2*N);

    // loop over entire simulation
    for (int i=0; i<until; i++) {

        // sub-loop (save summary statistics periodically, outside of this loop)
        for (int j=0; j<every; j++) {
            mutate();
            erode();
            resample_population();
        }
        cout << i << '\t' << get_mean_activity() << '\t' << get_diversity() << '\n';
    }
}

