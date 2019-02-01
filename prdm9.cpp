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

// CONSTANT PARAMETERS OF THE MODEL

// total number of fingers per array (fixed)
static const unsigned int Nfinger = 10;
// number of residues per finger
static const unsigned int Nres = 3;
// number of binding fingers (first in the list)
static const unsigned int Nbind = 3;
// number of amino-acids
static const unsigned int Naa = 10;

// DATA STRUCTURES 

// counts of all alleles of current population (cleaned up of absent alleles after each cycle)
map<vector<int>, int> allele_count;
// activities of all allelic classes ever visited during simulation
map<vector<int>, double> allelic_class_activity;

map<vector<int>, double> allelic_class_A;
map<vector<int>, double> allelic_class_A2;
map<vector<int>, double> zn_finger_A;
map<vector<int>, double> zn_finger_A2;


// OPERATIONS ON ALLELES AND ON POPULATION

// attempt to make a new allele and add count copies in current population
// check whether allele already exists in current population (in which case just add count)
//  check whether allelic class already exists; if not, then create it with an activity of 1.0
void make_new_allele(const vector<int>& allele, int count) {

    auto it = allele_count.find(allele);
    if (it == allele_count.end())   {
        allele_count[allele] = count;
    }
    else    {
        it->second += count;
    }

    auto allelic_class = vector<int>(allele.begin(), allele.begin() + Nbind*Nres);
    auto jt = allelic_class_activity.find(allelic_class);
    if (jt == allelic_class_activity.end()) {
        allelic_class_activity[allelic_class] = 1.0;
        allelic_class_A[allelic_class] = 0;
        allelic_class_A2[allelic_class] = 0;
    }
}

// return activity of given allele
// without changing data structures
// (i.e. if allele does not correspond to any existing class, return 1)
double get_activity(const vector<int>& allele) {
    auto allelic_class = vector<int>(allele.begin(), allele.begin() + Nbind*Nres);
    auto it = allelic_class_activity.find(allelic_class);
    if (it == allelic_class_activity.end()) {
        return 1.0;
    }
    return it->second;
}

// make random point mutation, return mutated allele 
vector<int> point_mutate(const vector<int>& allele) {
    vector<int> new_allele(allele);
    // choose position
    int pos = choose(allele.size());
    // choose new amino acid (possibly equal to old one)
    new_allele[pos] = choose(Naa);
    return new_allele;
}

// make random conversion event, return mutated allele
vector<int> conv_mutate(const vector<int>& allele)  {
    vector<int> new_allele(allele);
    int finger1 = choose(Nfinger);
    int finger2 = choose(Nfinger-1);
    if (finger2 >= finger1) {
        finger2++;
    }
    for (auto i=0; i<Nres; i++) {
        new_allele[finger2*Nres+i] = new_allele[finger1*Nres+i];
    }
    return new_allele;
}

// get fitness of an allele
// (fitness is averaged over all possible heterozygous backgrounds from the current population)
double get_fitness(const vector<int>& allele, double alpha)   {
    double x = get_activity(allele);
    double mean = 0;
    double tot = 0;
    for (auto& p : allele_count)    {
        double y = get_activity(p.first);
        mean += p.second * exp(alpha * log((x+y)/2));
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

// TRACING AND SUMMARY STATISTICS
// write allele to stream
void print_allele(ostream& os, const vector<int>& allele)   {
    for (auto i : allele) {
        os << i;
    }
}

// get mean recombination activity in current population
double get_mean_activity()  {
    double mean = 0;
    double tot = 0;
    for (auto& p : allele_count)    {
        mean += p.second * get_activity(p.first);
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

// get effective number of ZFs in an allele
double get_zf_diversity(const vector<int>& allele)  {

    map<vector<int>, double> freq;
    for (int i=0; i<Nfinger; i++)   {
        freq[vector<int>(allele.begin() + Nres*i, allele.begin() + Nres*(i+1))] ++;
    }
    double m2 = 0;
    for (auto& p : freq) {
        double f = p.second / Nfinger;
        m2 += f*f;
    }
    return 1.0 / m2;
}

// get mean effective number of ZFs in current population
double get_mean_zf_diversity()  {

    double mean = 0;
    double tot = 0;
    for (auto& p : allele_count)    {
        mean += p.second * get_zf_diversity(p.first);
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

// get effective number of alleles in current population
double get_allele_diversity()   {

    double tot = 0;
    for (auto& p : allele_count)    {
        tot += p.second;
    }
    double m2 = 0;
    for (auto& p : allele_count)    {
        double f = ((double) p.second) / tot;
        m2 += f*f;
    }
    return 1.0 / m2;
}

// get effective number of allelic classes in current population
double get_allelic_class_diversity()    {

    map<vector<int>,int> class_count;
    int tot = 0;
    for (auto& p : allele_count)    {
        auto allelic_class = vector<int>(p.first.begin(), p.first.begin() + Nbind*Nres);
        class_count[allelic_class] += p.second;
        tot += p.second;
    }
    double m2 = 0;
    for (auto& p : class_count) {
        double f = ((double) p.second) / tot;
        m2 += f*f;
    }
    return 1.0 / m2;
}

void push_current_allelic_class_freqs() {

    map<vector<int>,int> class_count;
    int tot = 0;
    for (auto& p : allele_count)    {
        auto allelic_class = vector<int>(p.first.begin(), p.first.begin() + Nbind*Nres);
        class_count[allelic_class] += p.second;
        tot += p.second;
    }
    for (auto& p : class_count) {
        double f = ((double) p.second) / tot;
        allelic_class_A[p.first] += f;
        allelic_class_A2[p.first] += f*f;
    }
}

// MAIN PROGRAM

int main(int argc, char* argv[])    {

    // parsing command line
    // population size (diploids: 2N alleles)
    int N = atoi(argv[1]);
    // scaled point mutation rate (per amino-acid position)
    double U = atof(argv[2]);
    // scaled conversion rate (per finger)
    double C = atof(argv[3]);
    // scaled erosion rate
    double rho = atof(argv[4]);
    // selection strength
    double alpha = atof(argv[5]);

    // simulation parameters
    int burnin = atoi(argv[6]);
    int every = atoi(argv[7]);
    int until = atoi(argv[8]);

    // name of simulation experiment
    string name = argv[9];

    // non-scaled point mutation rate and conversion rate per allele
    // Naa/(Naa-1): to allow for 'silent' point mutations
    double u = U / 4 / N * Nres * Nfinger * Naa / (Naa-1);
    double c = C / 4 / N * Nfinger;
    if (u >= 1.0)   {
        cerr << "error: total mutation rate per array is more than 1\n";
        exit(1);
    }
    if (c >= 1.0)   {
        cerr << "error: total conversion rate per array is more than 1\n";
        exit(1);
    }

    // initialize population
    vector<int> founder(Nfinger*Nres,0);
    make_new_allele(founder,2*N);

    /*
    // prepare output streams
    ofstream tos((name + ".trace").c_str());
    ofstream os((name + ".comp").c_str());
    */

    // summary statistic computed on the fly
    // mean allelic diversity
    double meandiv = 0;
    // mean allelic class diversity
    double meanclassdiv = 0;
    // mean recombination activity
    double meanrec = 0;
    // mean ZF diversity
    double meanzfdiv = 0;
    // number of iterations for which statistics were saved
    double nrep = 0;

    // loop over entire simulation
    for (int i=0; i<until; i++) {

        // sub-loop (save summary statistics periodically, outside of this loop)
        for (int j=0; j<every; j++) {

            // erode targets
            for (auto& p : allele_count)    {
                // grep allelic class
                auto allelic_class = vector<int>(p.first.begin(), p.first.begin() + Nbind*Nres);
                auto it = allelic_class_activity.find(allelic_class);
                if (it == allelic_class_activity.end()) {
                    cerr << "error: allelic class does not exist\n";
                    exit(1);
                }
                // erode activity at a rate proportional to allele frequency
                it->second *= exp(-rho*p.second/2/N);
            }

            // resample population

            // compute expected frequencies in next generation
            // as a function of current counts and fitness 
            vector<double> freqs;
            freqs.reserve(allele_count.size());
            double tot = 0;
            for (auto& p : allele_count)    {
                double f = p.second * get_fitness(p.first,alpha);
                freqs.push_back(f);
                tot += f;
            }
            // renormalize frequencies
            for (auto& f : freqs)  {
                f /= tot;
            }
            // draw counts for next gen in a temporary vector of ints
            vector<int> next_pop = multinomial_draw(2*N,freqs);
            // set new counts based on this temp vector
            // at this stage, some alleles might end up having a count == 0
            unsigned int i=0;
            for (auto& p : allele_count)    {
                p.second = next_pop[i++];
            }
            
            // point mutate
            for (auto& p : allele_count)    {
                // choose number of copies of the current allele that will undergo a point mutation
                int n = binomial_draw(p.second,u);
                // discount mutants from current count
                p.second -= n;
                // create the n mutants and add them to the population
                for (int k=0; k<n; k++) {
                    vector<int> new_allele = point_mutate(p.first);
                    make_new_allele(new_allele,1);
                }
            }

            // conv mutate
            for (auto& p : allele_count)    {
                // choose number of copies of the current allele that will undergo a conversion mutation
                int n = binomial_draw(p.second,c);
                // discount mutants from current count
                p.second -= n;
                // create the n mutants and add them to the population
                for (int k=0; k<n; k++) {
                    vector<int> new_allele = conv_mutate(p.first);
                    make_new_allele(new_allele,1);
                }
            }

            // clean up current list of alleles
            // of all alleles that have a count == 0
            auto it = allele_count.begin();
            while (it != allele_count.end())    {
                if (!it->second)    {
                    it = allele_count.erase(it);
                }
                else    {
                    it++;
                }
            }
        }

        // trace and save summary statistics
        if (i>=burnin)  {

            /*
            // trace current state
            tos << get_mean_activity() << '\t' << get_allele_diversity() << '\n';
            tos.flush();
            
            os << allele_count.size();
            for (auto& p : allele_count)    {
                os << '\t';
                print_allele(os,p.first);
                os << '\t' << p.second;
                os << '\t' << get_activity(p.first);
            }
            os << '\n';
            os.flush();
            */

            push_current_allelic_class_freqs();

            // save summary statistics
            double R = get_mean_activity();
            meanrec += R;
            meandiv += get_allele_diversity();
            meanclassdiv += get_allelic_class_diversity();
            meanzfdiv += get_mean_zf_diversity();
            nrep++;
        }
    }

    meanrec /= nrep;
    meandiv /= nrep;
    meanclassdiv /= nrep;
    meanzfdiv /= nrep;

    double totA = 0;
    double totA2 = 0;
    int totcount = 0;
    for (auto& p : allelic_class_A)  {
        totA += allelic_class_A[p.first];
        totA2 += allelic_class_A2[p.first];
        totcount++;
    }
    double tau1 = totA2 / totA * every;
    double tau2 = totA / totcount * every;

    ofstream sos((name + ".summary").c_str());
    sos << U << '\t' << C << '\t' << rho << '\t' << alpha << '\t' << meanrec << '\t' << meandiv << '\t' << meanclassdiv << '\t' << meanzfdiv << '\n';
    cout << U << '\t' << C << '\t' << rho << '\t' << alpha << '\t' << meanrec << '\t' << meandiv << '\t' << meanclassdiv << '\t' << meanzfdiv << '\n';
    cout << tau2 << '\t' << tau2 * meanclassdiv << '\n';
    cout << tau1 << '\t' << tau1 * meanclassdiv << '\n';


}

