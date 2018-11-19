using Random
using Printf
using Combinatorics
using Statistics
using GR
using Dates

@enum Choice C=1 D=2
ChoiceOrNot = Union{Choice, Nothing}

R = 3; S = 0
T = 5; P = 1
payoff_matrix = [R S
				 T P]
payoff_matrix = payoff_matrix .- mean(payoff_matrix)
println("Payoff Matrix: ", payoff_matrix)

yearly_tax = 0
println("Yearly Tax: ", yearly_tax)

payoff(a::ChoiceOrNot, b::ChoiceOrNot) =
    (a == nothing || b == nothing) ? 0 :
    payoff_matrix[Int(a), Int(b)]

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

memory1(s::Strategy) = Strategy("m" * s.name,
(memory, b_ID) -> begin
	b_ind = findfirst(==(b_ID), memory[2:end])
	if b_ind != nothing
		b_ind += 1
		memory[3:b_ind] = memory[2:b_ind-1]
		memory[2] = b_ID
		return nothing
	else
		memory[3:end] = memory[2:end-1]
		memory[2] = b_ID
		return s.decision(memory, b_ID)
	end
end,
(memory, b_choice) -> begin
	s.learning(memory, b_choice)
	if b_choice == C || b_choice == nothing
		memory[2:end-1] = memory[3:end]
		memory[end] = 0
	end
end)

memory2(s::Strategy) = Strategy("m" * s.name,
(memory, b_ID) -> begin
	b_ind = findfirst(==(b_ID), memory[2:end])
	if b_ind != nothing
		b_ind += 1
		memory[3:b_ind] = memory[2:b_ind-1]
		memory[2] = b_ID
		return nothing
	else
		empty_ind = findfirst(==(0), memory[2:end])
		if empty_ind != nothing
			empty_ind += 1
			memory[3:empty_ind] = memory[2:empty_ind-1]
		end
		memory[2] = b_ID
		return s.decision(memory, b_ID)
	end
end,
(memory, b_choice) -> begin
	s.learning(memory, b_choice)
	if b_choice == C || b_choice == nothing
		memory[2:end-1] = memory[3:end]
		memory[end] = 0
	end
end)

initial_census = Dict(
	ALLD => 40,
	ALLC => 0,
	memory2(ALLC) => 15,
	GRIM => 0,
	TFT  => 0,
	GTFT => 0,
	memory1(GTFT) => 0,
)

memory_size = initial_census[ALLD]
capacity = 50

newborn_HP = 100
fruitful_HP = 2 * newborn_HP
starving_HP = 0 # newborn_HP / 2

ID_counter = 0
new_ID() = global ID_counter += 1

mutable struct Agent
	strategy :: Strategy
	memory :: Array{Int}
	HP :: Float64
	ID :: Int
end

Agent(s::Strategy) = Agent(s, zeros(Int, memory_size),
						   newborn_HP, new_ID())
Agent(p::Agent) = Agent(p.strategy)

decision(a::Agent, b_ID::Int)::ChoiceOrNot =
    a.strategy.decision(a.memory, b_ID)
inform(a::Agent, b_choice::ChoiceOrNot) =
    a.strategy.learning(a.memory, b_choice)

function versus(a::Agent, b::Agent)
	choice_a = decision(a, b.ID)
	choice_b = decision(b, a.ID)

	a.HP += payoff(choice_a, choice_b)
	b.HP += payoff(choice_b, choice_a)

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
	for a in agents
		a.HP -= yearly_tax
	end
	newborn = Agent[]
	for parent in filter(fruitful, agents)
		parent.HP -= newborn_HP
		push!(newborn, Agent(parent))
	end
	agents = filter(!starving, [agents; newborn])
	overcapacity = length(agents) - capacity
	if overcapacity > 0
		selection = trues(length(agents))
		selection[1:overcapacity] .= false
		agents = agents[shuffle(selection)]
	end
	return agents
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

function existence(agents, years)
	return Channel((c) -> begin
					   global years
					   put!(c, census(agents))
					   for y in 1:years
						   if length(agents) <= 1
							   years = y - 1
							   break
						   end
						   agents = year(agents)
						   put!(c, census(agents))
					   end
				   end)
end

population(census) = [Agent(strategy)
					  for (strategy, n) in census
					  for i in 1:n]

function print_census(census::Dict)
	for (strategy, (n, HP)) in census
		@printf("%5s: %3d - %4d - %5.1f\n", strategy.name, n, HP, HP / n)
	end
end

function print_census(agents::Array{Agent})
	print_census(census(agents))
end

function print_census(title::String, x::Any) 
	println(title)
	print_census(x)
end

years = 25

function plot_censi(censi::Dict{Strategy,Array})
	fname = Dates.format(now(), "yyyy_mm_dd__HH_MM_SS_s") * ".png"
	beginprint(fname)

	snames = (s.name for s in keys(censi))

	labels = vec(permutedims(snames .* [" #" " HP"], (2, 1)))
	vals = hcat(values(censi)...)[1:years+1, :]

	snumbers = vals[:, 1:2:end]
	sHPs = vals[:, 2:2:end]

	plot(0:years, snumbers,
		 # ylim=(0, maximum(y_values)),
		 labels=(snames .* " #"))

	endprint()

	println("Plot is also available on $fname")
	print("Press Enter to continue...")
	read(stdin, Char)
end

censi = Dict{Strategy,Array}()

initial_population = population(initial_census)
exist = existence(initial_population, years)
for (yearp1, census) in enumerate(exist)
	print_census("Year #$(yearp1 - 1)", census)

	for (strategy, vals) in census
		if !haskey(censi, strategy)
			censi[strategy] = zeros(years + 1, 2)
		end
		censi[strategy][yearp1, :] = vals ./ [1, 100]
	end
end

plot_censi(censi)
