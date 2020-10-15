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

// CONSTANT PARAMETERS OF THE MODEL

// total number of fingers per array (fixed)
static const unsigned int Nfinger = 10;
// number of residues per finger
static const unsigned int Nres = 3;
// number of binding fingers (first in the list)
static const unsigned int Nbind = 3;
// number of amino-acids
static const unsigned int Naa = 5;

static double invasion_cutoff = 0.01;
static double zf_invasion_cutoff = invasion_cutoff;
// static double zf_invasion_cutoff = invasion_cutoff / Nfinger;
// detailed output deactivated 
static int write_history = 2;
static int gene_conversion_model = 1;
static int max_class = 1000;
static int max_zf = 1000;

static double invMeanTractLength = 0.14;
// another format for writing history of zfs and allelic classes
// static int write_history = 2;
// static int max_class = 40;
// static int max_zf = 10;

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

//geometric
int geometric_draw(double p){
    geometric_distribution<int> geomDis(p);
    return geomDis(e);
}

// DATA STRUCTURES 

// counts of all alleles of current population (cleaned up of absent alleles after each cycle)
map<vector<int>, int> allele_count;
// activities of all allelic classes ever visited during simulation
map<vector<int>, double> allelic_class_activity;

map<vector<int>, double> allelic_class_maxfreq;
map<vector<int>, double> allelic_class_cutoff_activity;
map<vector<int>, int> allelic_class_cutoff_moment;

map<vector<int>, double> zf_maxfreq;
map<vector<int>, int> zf_index;
map<vector<int>, double> zf_cutoff_activity;
map<vector<int>, int> zf_cutoff_moment;

map<vector<int>, int> zf_bind;

//GLOBAL VARIABLES
int totalConvertedResidues = 0;

int totalPointMuts = 0;  
int totalAlleleEffPointMuts = 0;
int totalAllelicClassEffPointMuts = 0;

int totalGeneConvMuts = 0;
int totalAlleleEffGeneConvMuts = 0;
int totalAllelicClassEffGeneConvMuts = 0;

// OPERATIONS ON ALLELES AND ON POPULATION

// attempt to make a new allele and add count copies in current population
// check whether allele already exists in current population (in which case just add count)
// check whether allelic class already exists; if not, then create it with an activity of 1.0
void make_new_allele(const vector<int>& allele, int count, int type) {

    if(type == 1){
        totalPointMuts++;
    }   
    else if(type == 2){
         totalGeneConvMuts++;   
    }
    auto it = allele_count.find(allele);
    if (it == allele_count.end())   {
        allele_count[allele] = count;
        if(type == 1){
            totalAlleleEffPointMuts++;
        }else if(type == 2){
            totalAlleleEffGeneConvMuts++;
        }
    }
    else    {
        allele_count[allele] += count;
    }

    auto allelic_class = vector<int>(allele.begin(), allele.begin() + Nbind*Nres);
    auto jt = allelic_class_activity.find(allelic_class);
    if (jt == allelic_class_activity.end()) {
        allelic_class_activity[allelic_class] = 1.0;
        // not quite 0, but below threshold anyway
        allelic_class_maxfreq[allelic_class] = 0;
        allelic_class_cutoff_activity[allelic_class] = 1.0;
        if(type == 1){
            totalAllelicClassEffPointMuts++;
        }else if(type == 2){
            totalAllelicClassEffGeneConvMuts++;
        }
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
    new_allele[pos] = choose(Naa)+1;

    // identify the (possibly new) zinc finger just created
    // and initialize the corresponding summary stats
    int i = pos / Nres;
    vector<int> finger(allele.begin() + Nres*i, allele.begin() + Nres*(i+1));
    auto it = zf_maxfreq.find(finger);
    if (it == zf_maxfreq.end())    {
        zf_maxfreq[finger] = 0;
        zf_index[finger] = i;
        zf_cutoff_activity[finger] = 1;
    }

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


// make random conversion event, return mutated allele
vector<int> conv_mutate_2(const vector<int>& allele)  {
    vector<int> new_allele(allele);
    int beginDonorFinger = choose(Nfinger);
    int beginAcceptorFinger = choose(Nfinger-1);
    int conversionLength = geometric_draw(invMeanTractLength)+1;
    //cout << conversionLength << "\t";
    int beginResidue = choose(Nres);
    
    int convertedResidues = 0;
    int beginDonorPos = beginDonorFinger*Nres+beginResidue;
    int beginAcceptorPos = beginAcceptorFinger*Nres+beginResidue;
    int acceptorPos = beginAcceptorPos;

    if(beginAcceptorFinger < beginDonorFinger){
        while(convertedResidues<conversionLength && acceptorPos<beginDonorPos && (beginDonorPos+convertedResidues)<(Nfinger*Nres)){
            new_allele[acceptorPos] = new_allele[beginDonorPos+convertedResidues];
            //cout << "newAllele[" << acceptorPos << "]=newAllele[" << beginDonorPos+convertedResidues << "]\n";
            convertedResidues++;
            acceptorPos++; 
        } 
    }else{
        beginAcceptorPos = (beginAcceptorFinger+1)*Nres+beginResidue;
        acceptorPos = beginAcceptorPos;
        while(convertedResidues<conversionLength && beginAcceptorPos>(beginDonorPos+convertedResidues) && acceptorPos<(Nfinger*Nres)){
            new_allele[acceptorPos] = new_allele[beginDonorPos+convertedResidues];
            //cout << "newAllele[" << acceptorPos << "]=newAllele[" << beginDonorPos+convertedResidues << "]\n";
            convertedResidues++;
            acceptorPos++; 
        }
    }
    totalConvertedResidues+=convertedResidues;
    return new_allele;
}

// get fitness of an allele
// (fitness is averaged over all possible heterozygous backgrounds from the current population)
double get_fitness(double activity, double alpha)   {
    double mean = 0;
    double tot = 0;
    for (auto& p : allele_count)    {
        double y = get_activity(p.first);
        mean += p.second * exp(alpha * log((activity + y) / 2));
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

double get_mean_fitness(double alpha)   {
    double mean = 0;
    double tot = 0;
    for (auto& p : allele_count)    {
        double x = get_activity(p.first);
        mean += p.second * get_fitness(x,alpha);
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
    for (auto p : allele_count)    {
        mean += p.second * get_activity(p.first);
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

// get effective number of ZFs in an allele
double get_zf_diversity(const vector<int>& allele)  {

    // finger counts across array
    map<vector<int>, double> freq;
    for (int i=0; i<Nfinger; i++)   {
        vector<int> finger(allele.begin() + Nres*i, allele.begin() + Nres*(i+1));
        auto it = freq.find(finger);
        if (it == freq.end())   {
            freq[finger] = 1.0;
        }
        else    {
            freq[finger] ++;
        }
    }
    // get zf diversity
    double m2 = 0;
    for (auto p : freq) {
        double f = p.second / Nfinger;
        m2 += f*f;
    }
    // return inverse of diversity
    return 1.0 / m2;
}

// get mean effective number of ZFs in current population
double get_mean_zf_diversity()  {

    double mean = 0;
    double tot = 0;
    for (auto p : allele_count)    {
        mean += p.second * get_zf_diversity(p.first);
        tot += p.second;
    }
    mean /= tot;
    return mean;
}

// get effective number of alleles in current population
double get_allele_diversity()   {

    double tot = 0;
    for (auto p : allele_count)    {
        tot += p.second;
    }
    double m2 = 0;
    for (auto p : allele_count)    {
        double f = ((double) p.second) / tot;
        m2 += f*f;
    }
    return 1.0 / m2;
}

// get effective number of allelic classes in current population
double get_allelic_class_diversity()    {

    map<vector<int>,int> class_count;
    int tot = 0;
    for (auto p : allele_count)    {
        auto allelic_class = vector<int>(p.first.begin(), p.first.begin() + Nbind*Nres);
        auto it = class_count.find(allelic_class);
        if (it == class_count.end())    {
            class_count[allelic_class] = p.second;
        }
        else    {
            class_count[allelic_class] += p.second;
        }
        tot += p.second;
    }
    double m2 = 0;
    for (auto p : class_count) {
        double f = ((double) p.second) / tot;
        m2 += f*f;
    }
    return 1.0 / m2;
}

void push_current_allelic_class_freqs(ostream& hos, ostream& aos, ostream& mos, int generation) {

    map<vector<int>,int> class_count;
    map<vector<int>,double> class_activity;
    int tot = 0;
    for (auto p : allele_count)    {
        auto allelic_class = vector<int>(p.first.begin(), p.first.begin() + Nbind*Nres);
        auto it = class_count.find(allelic_class);
        if (it == class_count.end())    {
            class_count[allelic_class] = p.second;
            class_activity[allelic_class] = p.second * get_activity(p.first);
        }
        else    {
            class_count[allelic_class] += p.second;
            class_activity[allelic_class] += p.second * get_activity(p.first);
        }
        tot += p.second;
    }

    int nclass = 0;
    for (auto p : class_count) {
        double f = ((double) p.second) / tot;
        class_activity[p.first] /= p.second;
        double act = ((double) class_activity[p.first]);
        if (allelic_class_maxfreq[p.first] < f) {
            if ((f >= invasion_cutoff) && (allelic_class_maxfreq[p.first] < invasion_cutoff)) {
                allelic_class_cutoff_activity[p.first] = class_activity[p.first];
                allelic_class_cutoff_moment[p.first] = generation;
                for (int i=0; i<Nbind; i++)   {
                    vector<int> finger(p.first.begin() + Nres*i, p.first.begin() + Nres*(i+1));
                    zf_bind[finger] = 1;
                }
            }
            allelic_class_maxfreq[p.first] = f;
        }
        if (write_history == 1)  {
            for (auto i : p.first) {
                hos << i;
            }
            hos << '\t';
            aos << f << '\t';
            mos << act << '\t';
            nclass++;
        }
        if ((write_history == 2) && (f >= invasion_cutoff)) {
            for (auto i : p.first) {
                hos << i;
            }
            hos << "\t";
            nclass++;
        }
    }
    if (write_history)  {
        if (nclass > max_class) {
            cerr << "error: overflow in class file\n";
            exit(1);
        }
        for (int i=nclass; i<max_class; i++)    {
            if (write_history == 1) {
                hos << 0 << '\t';
                aos << 0 << '\t';
                mos << 0 << '\t';
            }
            if (write_history == 2) {
                for (int k=0; k<Nbind*Nres; k++)    {
                    hos << " ";
                }
                hos << " ";
            }
        }
        hos << '\n';
        hos.flush();
        if (write_history == 1) {
            aos << '\n';
            aos.flush();
            mos << '\n';
            mos.flush();
             
        }
    }
}

// returns effective number of zn fingers in current population
double push_current_zf_freqs(ostream& hos, ostream& aos, int generation, int limit) {

    map<vector<int>,int> zf_count;
    map<vector<int>,double> zf_activity;
    int tot = 0;
    for (auto p : allele_count)    {
        double allele_activity = get_activity(p.first);
        for (int i=0; i<limit; i++)   {
            vector<int> finger(p.first.begin() + Nres*i, p.first.begin() + Nres*(i+1));
            auto it = zf_count.find(finger);
            if (it == zf_count.end())   {
                zf_count[finger] = p.second;
                zf_activity[finger] = p.second * allele_activity;
            }
            else    {
                zf_count[finger] += p.second;
                zf_activity[finger] += p.second * allele_activity;
            }
            tot += p.second;
        }
    }

    int nzf = 0;
    double m2 = 0;
    // double mean_zf_activity = 0;
    for (auto p : zf_count) {
        double f = ((double) p.second) / tot;
        zf_activity[p.first] /= p.second;
        m2 += f*f;
        if (zf_maxfreq[p.first] < f) {
            if ((f >= zf_invasion_cutoff) && (zf_maxfreq[p.first] < zf_invasion_cutoff)) {
                zf_cutoff_activity[p.first] = zf_activity[p.first];
                zf_cutoff_moment[p.first] = generation;
            }
            zf_maxfreq[p.first] = f;
        }
        if (write_history == 1)  {
            // print allelic class code
            for (auto i : p.first) {
                hos << i;
            }
            hos << '\t';
            aos << f << '\t';
            nzf++;
        }
        if ((write_history == 2) && (f >= zf_invasion_cutoff)) {
            // print allelic class code
            for (auto i : p.first) {
                hos << i;
            }
            hos << " ";
            nzf++;
        }
    }
    if (write_history)  {
        if (nzf > max_zf) {
            cerr << "error: overflow in zf class file\n";
            exit(1);
        }
        for (int i=nzf; i<max_zf; i++)    {
            if (write_history == 1) {
                hos << 0 << '\t';
                aos << 0 << '\t';
            }
            if (write_history == 2) {
                for (int k=0; k<Nres; k++)  {
                    hos << " ";
                }
                hos << " ";
            }
        }
        hos << '\n';
        hos.flush();
        if (write_history == 1) {
            aos << '\n';
            aos.flush();
        }
    }
    return 1.0 / m2;
}


// returns effective number of zn fingers in current population
/*double push_current_zf_freqs(ostream& hos, ostream& aos, int generation) {

    map<vector<int>,int> zf_count;
    map<vector<int>,double> zf_activity;
    int tot = 0;
    for (auto p : allele_count)    {
        double allele_activity = get_activity(p.first);
        for (int i=0; i<Nfinger; i++)   {
            vector<int> finger(p.first.begin() + Nres*i, p.first.begin() + Nres*(i+1));
            auto it = zf_count.find(finger);
            if (it == zf_count.end())   {
                zf_count[finger] = p.second;
                zf_activity[finger] = p.second * allele_activity;
            }
            else    {
                zf_count[finger] += p.second;
                zf_activity[finger] += p.second * allele_activity;
            }
            tot += p.second;
        }
    }

    int nzf = 0;
    double m2 = 0;
    // double mean_zf_activity = 0;
    for (auto p : zf_count) {
        double f = ((double) p.second) / tot;
        zf_activity[p.first] /= p.second;
        m2 += f*f;
        if (zf_maxfreq[p.first] < f) {
            if ((f >= zf_invasion_cutoff) && (zf_maxfreq[p.first] < zf_invasion_cutoff)) {
                zf_cutoff_activity[p.first] = zf_activity[p.first];
                zf_cutoff_moment[p.first] = generation;
            }
            zf_maxfreq[p.first] = f;
        }
        if (write_history == 1)  {
            // print allelic class code
            for (auto i : p.first) {
                hos << i;
            }
            hos << '\t';
            aos << f << '\t';
            nzf++;
        }
        if ((write_history == 2) && (f >= zf_invasion_cutoff)) {
            // print allelic class code
            for (auto i : p.first) {
                hos << i;
            }
            hos << " ";
            nzf++;
        }
    }
    if (write_history)  {
        if (nzf > max_zf) {
            cerr << "error: overflow in zf class file\n";
            exit(1);
        }
        for (int i=nzf; i<max_zf; i++)    {
            if (write_history == 1) {
                hos << 0 << '\t';
                aos << 0 << '\t';
            }
            if (write_history == 2) {
                for (int k=0; k<Nres; k++)  {
                    hos << " ";
                }
                hos << " ";
            }
        }
        hos << '\n';
        hos.flush();
        if (write_history == 1) {
            aos << '\n';
            aos.flush();
        }
    }
    return 1.0 / m2;
}
*/
// returns effective number of zn fingers in current population
double get_zfdivpop()   {

    map<vector<int>,int> zf_count;
    int tot = 0;
    for (auto p : allele_count)    {
        for (int i=0; i<Nfinger; i++)   {
            vector<int> finger(p.first.begin() + Nres*i, p.first.begin() + Nres*(i+1));
            auto it = zf_count.find(finger);
            if (it == zf_count.end())   {
                zf_count[finger] = p.second;
            }
            else    {
                zf_count[finger] += p.second;
            }
            tot += p.second;
        }
    }

    double m2 = 0;
    for (auto p : zf_count) {
        double f = ((double) p.second) / tot;
        m2 += f*f;
    }
    return 1.0 / m2;
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
    for(int j = 0; j<Naa; j++){
        vector<int> founder(Nfinger*Nres,j+1);
        make_new_allele(founder, 2*N/Naa, 0);
    }
    // summary statistic computed on the fly
    // mean allelic diversity
    double meandiv = 0;
    // mean allelic class diversity
    double meanclassdiv = 0;
    // mean recombination activity
    double meanrec = 0;
    // mean ZF diversity
    double meanzfdivarray = 0;
    double meanzfdivpop = 0;
    // selection coefficient for new allele
    double means0 = 0;
    double meaninvs0 = 0;
    double meanlogs0 = 0;
    // number of iterations for which statistics were saved
    double nrep = 0;

    ofstream hos((name + ".allelic_class_history").c_str());
    ofstream aos((name + ".allelic_class_freq_history").c_str());
    ofstream mos((name + ".alelic_class_act_history").c_str());

    ofstream zhos((name + ".zf_history").c_str());
    ofstream zaos((name + ".zf_freq_history").c_str());

    ofstream zhosB((name + ".zf_history_bind").c_str());
    ofstream zaosB((name + ".zf_freq_history_bind").c_str());

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
            for (auto p : allele_count)    {
                double f = p.second * get_fitness(get_activity(p.first),alpha);
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
                    make_new_allele(new_allele, 1, 1);
                }
            }

            // conv mutate
            for (auto& p : allele_count)    {
                // choose number of copies of the current allele that will undergo a conversion mutation
                int n = binomial_draw(p.second,c);
                // discount mutants from current count
                p.second -= n;
                // create the n mutants and add them to the population
                if(gene_conversion_model == 1){
                    for (int k=0; k<n; k++) {
                        vector<int> new_allele = conv_mutate(p.first);
                        make_new_allele(new_allele, 1, 2);
                    }
                }else if(gene_conversion_model == 2){
                    for (int k=0; k<n; k++) {
                        vector<int> new_allele = conv_mutate_2(p.first);
                        make_new_allele(new_allele, 1, 2);
                    }
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

            push_current_allelic_class_freqs(hos,aos,mos,i-burnin);
            double zfdivpop = push_current_zf_freqs(zhos,zaos,i-burnin,Nfinger);
            double zfdivbind = push_current_zf_freqs(zhosB,zaosB,i-burnin,Nbind);

            // save summary statistics
            double R = get_mean_activity();
            meanrec += R;
            meandiv += get_allele_diversity();
            meanclassdiv += get_allelic_class_diversity();
            meanzfdivarray += get_mean_zf_diversity();
            meanzfdivpop += zfdivpop;
            double s0 = log(get_fitness(1.0,alpha) / get_mean_fitness(alpha));
            means0 += s0;
            meaninvs0 += 1.0 / s0;
            meanlogs0 += log(s0);
            nrep++;
        }
    }

    meanrec /= nrep;
    meandiv /= nrep;
    meanclassdiv /= nrep;
    meanzfdivarray /= nrep;
    meanzfdivpop /= nrep;

    means0 /= nrep;
    meaninvs0 /= nrep;
    meanlogs0 /= nrep;

    int totcount_cutoff = 0;
    double mean_allelic_class_cutoff_activity = 0;
    for (auto& p : allelic_class_maxfreq)  {
        if (p.second > invasion_cutoff)   {
            totcount_cutoff ++;
            mean_allelic_class_cutoff_activity += allelic_class_cutoff_activity[p.first];
        }
    }
    mean_allelic_class_cutoff_activity /= totcount_cutoff;

    // time between successive invasions per 2*N generations
    double tau_class = ((double) (until-burnin)) / totcount_cutoff * every / 2 / N;
    // invasion rate per 2*N generation
    double invrate_class = 1.0 / tau_class;

    int ztotcount_cutoff = 0;
    int within = 0;
    double mean_zf_cutoff_activity = 0;
    for (auto& p : zf_maxfreq)  {
        if (p.second > zf_invasion_cutoff)   {
            ztotcount_cutoff ++;
            if (zf_index[p.first] < Nbind)   {
                within++;
            }
            mean_zf_cutoff_activity += zf_cutoff_activity[p.first];
        }
    }
    mean_zf_cutoff_activity /= ztotcount_cutoff;
    double tau_z = ((double) (until-burnin)) / ztotcount_cutoff * every / 2 / N;
    // invasion rate per 2N generation
    double invrate_z = 1.0 / tau_z;
    double within_frac = ((double) within) / ztotcount_cutoff;

    int ztotcount2 = 0;
    for (auto& p : zf_bind)   {
        ztotcount2++;
    }
    double tau_z2 = ((double) (until-burnin)) / ztotcount2 * every / 2 / N;
    double invrate_z2 = 1.0 / tau_z2;

    // predicted number of allelic classes:
    double predclassdiv0 = meanzfdivpop * meanzfdivpop * meanzfdivpop;
    double predclassdiv = meanzfdivpop * meanzfdivpop * meanzfdivpop - meanzfdivpop * meanzfdivpop;

    // selection coefficient for new alleles
    // linearized approximation
    double meanS0 = 4 * N * means0;
    double meanharmS0 = 4 * N / meaninvs0;
    double meangeomS0 = 4 * N * exp(meanlogs0);
    double preds0 = alpha * (1-meanrec) / 2 / meanrec;
    double predS0 = 4 * N * preds0;
    // invasion rate per 2N generation
    // double predinvrate_z1 = 2 * N * Nres * Nbind * U * (means0 - c/Nfinger);
    double predinvrate_z = 2 * N * Nres * Nbind * U * means0;
    double predinvrate_z2 = 2 * N * Nres * Nbind * U / meaninvs0;
    double predinvrate_z3 = 2 * N * Nres * Nbind * U * exp(meanlogs0);

    double k = exp(log(meanclassdiv) / Nbind);
    double predinvrate_class = 2 * N * Nbind * meanzfdivpop / 2 * C * means0;
    // double predinvrate_class = 2 * N * Nbind * (meanzfdivpop - k - 1) * C * means0;
    double predinvrate_class2 = 2 * N * Nbind * (meanzfdivpop - k - 1) * C / meaninvs0;
    double predinvrate_class3 = 2 * N * Nbind * (meanzfdivpop - k - 1) * C * exp(meanlogs0);

    double realConvRate = ((double) (totalConvertedResidues)) / until / every / 2 / N / Nres;

    cout << "U\tC\trho\talpha\tmeanrecomb\tcl_div\tzf_div\tcl_inv\tzf_inv\tzf_inv2\t4Ns\t";
    cout << "predicted4Ns\tratioClassZnfInvRate1\tratioClassZnfInvRate2\tMaxClassDivBasedOnZnmfDiv1\tMaxClassDivBasedOnZnmfDiv2\tPredZnfInvRate\tPredClassInvRate\t";
    cout << "FracInvZnfBornWithinBindReg\tMeanCutoffActInvZnf\tMeanCutoffActInvClass\ttotalConvertedResidues\trealConvRate\ttheorConvRate\t";
    cout << "totalPointMuts\ttotalAlleleEffPointMuts\ttotalAllelicClassEffPointMuts\ttotalGeneConvMuts\ttotalAlleleEffGeneConvMuts\ttotalAllelicClassEffGeneConvMuts\t";
    cout << "meandiv\n";
 
    cout << U << '\t' << C << '\t' << rho << '\t' << alpha << '\t' << meanrec << '\t' << meanclassdiv << '\t' << meanzfdivpop << '\t' << invrate_class << '\t' << invrate_z << '\t' << invrate_z2 << '\t' << meanS0 << '\t';
    cout << predS0 << '\t' << invrate_class / invrate_z << '\t' << invrate_class / invrate_z2 << '\t' << predclassdiv0 << '\t' << predclassdiv << '\t' << predinvrate_z << '\t';
    cout << predinvrate_class << '\t' << '\t' << within_frac << '\t' << mean_zf_cutoff_activity << '\t' << mean_allelic_class_cutoff_activity << '\t';
    cout << totalConvertedResidues << '\t' << realConvRate << '\t' << c << '\t';
    cout << totalPointMuts << '\t' << totalAlleleEffPointMuts << '\t' << totalAllelicClassEffPointMuts << '\t' << totalGeneConvMuts << '\t' << totalAlleleEffGeneConvMuts << '\t' << totalAllelicClassEffGeneConvMuts << '\t';
    cout << meandiv << '\n';
 

    // cout << "U\tC\trho\talpha\tmeanrecomb\tcl_div\tzf_div\tcl_inv\tzf_inv\tzf_inv2\t4Ns\n";
    // cout << U << '\t' << C << '\t' << rho << '\t' << alpha << '\t' << meanrec << '\t' << meanclassdiv << '\t' << meanzfdivpop << '\t' << invrate_class << '\t' << invrate_z << '\t' << invrate_z2 << '\t' << meanS0 << '\n';
    // cout << '\n';
    // cout << "predicted 4Ns (linearized)                        : " << predS0 << '\n';
    // cout << "ratio of class / zf invasion rates                : " << invrate_class / invrate_z << '\t' << invrate_class / invrate_z2 << '\n';
    // cout << "max class div based on zf div                     : " << predclassdiv0 << '\t' << predclassdiv << '\n';
    // cout << "pred zf inv rate                                  : " << predinvrate_z << '\n';
    // cout << "pred class inv rate                               : " << predinvrate_class << '\n';
    // // cout << "ratio of predicted inv rates                      : " << predinvrate_class / predinvrate_z << '\n';
    // cout << '\n';
    // cout << "frac of invading zf created within binding region : " << within_frac << '\n';
    // cout << "mean cutoff activity of invading zfs              : " << mean_zf_cutoff_activity << '\n';
    // cout << "mean cutoff activity of invading allelic classes  : " << mean_allelic_class_cutoff_activity << '\n';
    // cout << '\n';

}