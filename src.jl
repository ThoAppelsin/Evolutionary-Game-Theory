using Random
using Printf
using Combinatorics
using Statistics

@enum Choice C=1 D=2

R = 3; S = 0
T = 5; P = 1
payoff_matrix = [R S
				 T P]
payoff_matrix = payoff_matrix .- mean(payoff_matrix)
println("Payoff Matrix: ", payoff_matrix)

payoff(a::Choice, b::Choice) = payoff_matrix[Int(a), Int(b)]

struct Strategy
	name::String
	decision::Function
	learning::Function
end

ALLD = Strategy("ALLD",
(memory, b_ID) -> D,
(memory, b_choice) -> nothing)

ALLC = Strategy("ALLC",
(memory, b_ID) -> C,
(memory, b_choice) -> nothing)

GRIM = Strategy("GRIM",
(memory, b_ID) -> memory[1] == 1 ? D : C,
(memory, b_choice) -> if b_choice == D; memory[1] = 1 end)

TFT = Strategy("TFT",
(memory, b_ID) -> memory[1] == Int(D) ? D : C,
(memory, b_choice) -> memory[1] = Int(b_choice))

generousness = min(1 - (T - R) / (R - S), (R - P) / (T - P))
GTFT = Strategy("GTFT",
(memory, b_ID) -> (memory[1] == Int(D) && rand() >= generousness) ? D : C,
(memory, b_choice) -> memory[1] = Int(b_choice))

memory(s::Strategy) = Strategy("memory " * s.name,
(memory, b_ID) -> begin
	if b_ID in memory[2:end]
		return
	end
	memory[3:end] = memory[2:end-1]
	memory[2] = b_ID
	return s.decision(memory, b_ID)
end
(memory, b_choice) ->
	

initial_census = Dict(
	ALLD => 1,
	ALLC => 0,
	GRIM => 0,
	TFT  => 0,
	GTFT => 20,
)

memory_size = 10

newborn_HP = 100
fruitful_HP = 1.5 * newborn_HP
starving_HP = 0 # newborn_HP / 2

ID_counter = 0
new_ID() = global ID_counter += 1

mutable struct Agent
	strategy :: Strategy
	memory :: Array{Int}
	HP :: Float64
	ID :: Int
end

Agent(s::Strategy) = Agent(s, zeros(Int, memory_size), newborn_HP, new_ID())
Agent(p::Agent) = Agent(p.strategy)

decision(a::Agent, b_ID::Int)::Choice = a.strategy.decision(a.memory, b_ID)
inform(a::Agent, b_choice::Choice) = a.strategy.learning(a.memory, b_choice)

function versus(a::Agent, b::Agent)
	choice_a = decision(a, b.ID)
	choice_b = decision(b, a.ID)

	a.HP += payoff(choice_a, choice_b)
	b.HP += payoff(choice_b, choice_a)

	#if a.strategy.name == "TFT" && choice_a == C
		#println("cooperation against ", b.strategy.name)
	#end
	#if b.strategy.name == "TFT" && choice_b == C
		#println("cooperation against ", b.strategy.name)
	#end

	inform(a, choice_b)
	inform(b, choice_a)
	return
end

fruitful(a::Agent)::Bool = a.HP >= fruitful_HP
starving(a::Agent)::Bool = a.HP <= starving_HP

function year(agents::Array{Agent})
	for (a, b) in shuffle(collect(combinations(agents, 2)))
		versus(a, b)
	end
	newborn = Agent[]
	for parent in filter(fruitful, agents)
		parent.HP -= newborn_HP
		push!(newborn, Agent(parent))
	end
	return filter(!starving, [agents; newborn])
end

function census(agents)
	census = Dict()
	for agent in agents
		if haskey(census, agent.strategy)
			census[agent.strategy] += [1, agent.HP]
		else
			census[agent.strategy] = [1, agent.HP]
		end
	end
	return census
end

function print_census(agents)
	for (strategy, (n, HP)) in census(agents)
		@printf("%4s: %3d - %4d\n", strategy.name, n, HP)
	end
end

function existence(agents, years)
	println("Year #0")
	print_census(agents)
	for y in 1:years
		agents = year(agents)
		println("Year #$(y)")
		print_census(agents)
	end
end

population(census) = [Agent(strategy)
					  for (strategy, n) in census
					  for i in 1:n]

existence(population(initial_census), 20)
