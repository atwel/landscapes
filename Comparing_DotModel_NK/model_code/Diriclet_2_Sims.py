from Landscapes import *
#from Experiment import Make_Lon
import numpy as np

def loc_avg_fitness(loc, bit_fits, our_dirich, reps=100):

    return sum([get_fitness(loc,bit_fits,our_dirich) for i in range(reps)])/reps



def get_fitness(current_loc, bit_fits, our_dirichlet):

    bits = bit_fits.loc["".join([str(i) for i in current_loc])].fitness
    weights = our_dirichlet.rvs()[0]

    return sum([i*j for i,j in zip(bits,weights)])

def bit_fitness(landscape, N, K):

    permutations = []
    bit_fits = []
    all_perms = list(itertools.product([0,1],repeat=N))
    for perm in all_perms:
        w_sub_i = []

        #Loop through all N
        for currIndex in np.arange(N):
            #get the fitness indices from each k based upon local gene values
            localgenes = perm[currIndex:currIndex+K+1]
            #loop through to next if were are near the nth index
            if currIndex+K+1 > N:
                localgenes = np.append(localgenes,perm[0:currIndex-(N-K)+1])
            #get index fitness  is stored at
            interactIndex = ((2**(np.arange(K+1)*(localgenes)))*localgenes).sum()
            #update fitness
            w_sub_i.append(landscape[currIndex,interactIndex])
        permutations.append("".join([str(i) for i in perm]))
        bit_fits.append(w_sub_i)

    a = {"fitness":pd.Series(bit_fits,index=permutations),
         "Location":pd.Series(all_perms,index=permutations)}
    return pd.DataFrame(a)


def random_walker(step_count, bit_fitnesses,our_dirich, N):

    #Set random starting position
    currPosition = (np.random.rand(N)>0.5).astype(int)

    #Get first fitness value
    FitnessHistory=[loc_avg_fitness(currPosition,bit_fitnesses, our_dirich)]
    for j in range(step_count):
        #Take a step by picking a random gene to change
        toChange = random.randint(0,N-1)
        #this changes 0 to 1 and 1 to zero
        currPosition[toChange] = abs(currPosition[toChange]-1)
        #Append to fitness data
        FitnessHistory.append(loc_avg_fitness(currPosition,bit_fitnesses, our_dirich))

    return FitnessHistory





def get_dirichlet_2_fitness(current_permutation,K,concentration_params):
    #generate a dirichlet distribution
    N=len(current_permutation)
    our_dirichlet = stats.dirichlet(concentration_params)
    #We only have one instation of the permutation: current_permutation
    genome_fitness = 0
    #get dirichlet draw

    #print(dir_draw)
    for currentCharacteristic in np.arange(N):
        #get the fitness indices from each k based upon local gene values
        localgenes = current_permutation[currentCharacteristic:currentCharacteristic+K+1]
        #print(localgenes)
        #loop through to next if were are near the nth index
        if currentCharacteristic+K+1 > N:
            #print(currentCharacteristic-(N-K)+1)
            localgenes = np.append(localgenes,current_permutation[0:currentCharacteristic-(N-K)+1])
            #print(localgenes)
            indices = list(range(currentCharacteristic,N)) + list(range(currentCharacteristic-(N-K)+1))
        else:
            indices = list(range(currentCharacteristic,currentCharacteristic+K+1))
        #get index fitness  is stored at
        interactIndex = ((2**(np.arange(K+1)*(localgenes)))*localgenes).sum()
       # print(indices)

        dir_draw = our_dirichlet.rvs()[0]
        currentValues= dir_draw[indices]
        #print(currentValues)
        #print(dir_draw[indices])
        #print(currentValues.sum())
        #print(dir_draw[indices].sum())
            #print(indices,currentValues)

        #add current value to the running fitness total

        genome_fitness += sum(currentValues)

   # print(genome_fitness)
    return(genome_fitness)



#create fitness history initialization
def get_iterated_hill_climb_dirichlet(Landscape,Neighbor_Distance,Step_Size,Iterations,our_dirichlet):
    #initalize random position
    N=len(Landscape.iloc[0].Location)
    Position= Position_rand(N)
    #draw weights
    weights = our_dirichlet.rvs()[0]
    FitnessHistory=[]
    PostionHistory=[]

    #record jumps down
    Jump_Down_history=[1]

    for j in range(Iterations):
        Position= Position_rand(N)
        #print(Position)

        Is_Maxima=0
        while(Is_Maxima<1):
            weights = our_dirichlet.rvs()[0]

            #print(Position)
            #Identify the Neighbors (distance of M from initial position row)
            Neighbors = Landscape[Landscape['Location'].apply(lambda row : sum(abs(np.array(row)-np.array(Landscape.loc["".join([str(i) for i in Position])].Location)))==Neighbor_Distance)]        #Get the maximum fitness value of the neighbors
            Neighbors.fitness = Neighbors.fitness.apply(lambda x: sum(x*weights))

            BetterNeighbors = Neighbors[Neighbors.fitness>sum(Landscape.loc["".join([str(i) for i in Position])].fitness*weights)]
            #randomize order of neighobrs then loop until one exceeds
            if len(BetterNeighbors)>0: #only update if there exists at least one superior neighobr
                BetterNeighbors.sample(frac=1)#randomly sample better neighbors
                Position = BetterNeighbors.index.values[0] #return the first index value
               # Jump_Down_history.append(0)

            #record maxima
            else:
               # print(Position)
                Is_Maxima=1
                #print('maxima')

                #Append to fitness data
            fit = sum(Landscape.loc["".join([str(i) for i in Position])].fitness*weights)

        FitnessHistory.append(fit)
        PostionHistory.append(Position[:])
        if j%100 ==1:
            print(j)
    return(PostionHistory,FitnessHistory)


#The jumps are recorded after the maxima as one
def get_maxima(Runs,Jumps):
    #Get index location of jumps (not including the starting position) and subtract by one, thats a maxima
    index_pos_list = [ i-1 for i in range(1,len(Jumps)) if Jumps[i] == 1 ]
    Runs=np.array(Runs)
    Maximas = np.array(Runs[index_pos_list])
    return(Maximas)
    #Get those index locations in the runs


params_symmetrical = [100,100,100,100,100,100,100,100,100,100]

params_symmetrical = [100,100,100,100,100,100,100,100,100,100]

N = 10
#If N=10, 2*2^N=2024
Num_Sims = 2*2**N
#Num_Sims = 20

concentration_params = params_symmetrical

Symmetrical_Runs_Fitness =[]
Symmetrical_Jumps= []
Symmetrical_History =[]

with open('Symmetrical_History_correct.txt', 'w') as filehandle:
    for K in range(10):
        landscape = np.random.rand(N, 2**(K+1))
        bit_fits = bit_fitness(landscape,N,K)
        our_dirich = stats.dirichlet(concentration_params)
        Current_History_Maxima, Current_Climb= get_iterated_hill_climb_dirichlet(bit_fits,1,2,Num_Sims,our_dirich)
    #    Symmetrical_Runs_Fitness.append(Current_Climb)
        #Symmetrical_Jumps.append(Current_Jumps)
    #    Symmetrical_History.append(Current_History_Maxima)
        filehandle.write('%s\n' % Current_History_Maxima)


###NOT the full number of sims
params_asymmetrical_neighbor = [100,100,100,1000,1000,100,100,100,100,100]

N = 10
#If N=10, 2*2^N=2024
Num_Sims = 2**2^N
#Num_Sims = 20

concentration_params = params_asymmetrical_neighbor

Asymmetrical_Neighbor_Runs_Fitness =[]
Asymmetrical_Neighbor_Jumps= []
Asymmetrical_Neighbor_History =[]
with open('Asymmetrical_Neighbor_History.txt', 'w') as filehandle:

    for K in range(10):
        landscape = np.random.rand(N, 2**(K+1))
        bit_fits = bit_fitness(landscape,N,K)
        our_dirich = stats.dirichlet(concentration_params)
        Current_History_Maxima, Current_Climb= get_iterated_hill_climb_dirichlet(bit_fits,1,2,Num_Sims,our_dirich)
    #    Asymmetrical_Neighbor_Runs_Fitness.append(Current_Climb)
        #Symmetrical_Jumps.append(Current_Jumps)
        #Asymmetrical_Neighbor_History.append(Current_History_Maxima)
        filehandle.write('%s\n' % Current_History_Maxima)

params_asymmetrical_nonneighbor = [1000,100,100,1000,100,1000,100,100,100,100]

N = 10
#If N=10, 2*2^N=2024
Num_Sims = 2*2**N
#Num_Sims = 20

concentration_params = params_asymmetrical_nonneighbor

Asymmetrical_Non_Neighbor_Runs_Fitness =[]
Asymmetrical_Non_Neighbor_Jumps= []
Asymmetrical_Non_Neighbor_History =[]
with open('Asymmetrical_Non_Neighbor_Jumps.txt', 'w') as filehandle:

    for K in range(10):
        landscape = np.random.rand(N, 2**(K+1))
        bit_fits = bit_fitness(landscape,N,K)
        our_dirich = stats.dirichlet(concentration_params)
        Current_Climb, Current_Jumps= get_iterated_hill_climb_dirichlet(bit_fits,1,2,Num_Sims,our_dirich)
        #Asymmetrical_Non_Neighbor_Runs_Fitness.append(Current_Climb)
        #Symmetrical_Jumps.append(Current_Jumps)
        #Asymmetrical_Non_Neighbor_Jumps.append(Current_History_Maxima)
        filehandle.write('%s\n' % Current_History_Maxima)
