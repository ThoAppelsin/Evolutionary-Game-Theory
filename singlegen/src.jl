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

~(::Nothing) = false
~(::Choice) = true
~(i::Int) = i ≠ 0

payoff(a::ChoiceOrNot, b::ChoiceOrNot) = ~a && ~b ? payoff_matrix[Int(a), Int(b)] : 0

struct Strategy
	name::String

	# (memory, id-of-opponent) -> choice
	decision::Function

	# (memory, choice-of-opponent) -> nothing
	learning::Function
end

ALLD = Strategy("ALLD", (_, _) -> D, (_, _) -> nothing)
ALLC = Strategy("ALLC", (_, _) -> C, (_, _) -> nothing)
GRIM = Strategy("GRIM",
				(m, _) -> m[1] == 1 ? D : C,
				(m, c) -> if c == D; m[1] = 1 end)

TFT_learning = (m, c) -> if ~c; m[1] = Int(c) end
TFT = Strategy("TFT",
			   (m, _) -> m[1] == Int(D) ? D : C,
			   TFT_learning)

generousness = min(1 - (T - R) / (R - S), (R - P) / (T - P))
GTFT = Strategy("GTFT",
				(m, _) -> (m[1] == Int(D) && rand() ≥ generousness) ? D : C,
				TFT_learning)

fifo_memory(s::Strategy) = Strategy("m" * s.name,
(m, id) -> if ~(i = findfirst(==(id), m[2:end]))
	i += 1
	m[3:i] = m[2:i-1]
	m[2] = id
	nothing
else
	m[3:end] = m[2:end-1]
	m[2] = id
	s.decision(m, id)
end,
(m, c) -> begin
	s.learning(m, c)
	if c ≠ D
		m[2:end-1] = m[3:end]
		m[end] = 0
	end
end)

filo_memory(s::Strategy) = Strategy("m" * s.name,
(m, id) -> if ~(id_i = findfirst(==(id), m[2:end]))
	id_i += 1
	m[3:id_i] = m[2:id_i-1]
	m[2] = id
	nothing
else
	if ~(empty_i = findfirst(==(0), m[2:end]))
		empty_i += 1
		m[3:empty_i] = m[2:empty_i-1]
	end
	m[2] = id
	s.decision(m, id)
end,
(m, c) -> begin
	s.learning(m, c)
	if c ≠ D
		m[2:end-1] = m[3:end]
		m[end] = 0
	end
end)

initial_census = Dict(
	ALLD => 40,
	ALLC => 0,
	filo_memory(ALLC) => 15,
	GRIM => 0,
	TFT  => 0,
	GTFT => 20,
	fifo_memory(GTFT) => 0,
)

memory_size = initial_census[ALLD] * 2
capacity = 50

newborn_HP = 100
HP_per_child = newborn_HP
starving_HP = 0

ID_counter = 0
new_ID() = global ID_counter += 1
reset_ID() = global ID_counter = 0

mutable struct Agent
	strategy :: Strategy
	memory :: Array{Int}
	HP :: Float64
	ID :: Int
end

Agent(s::Strategy) = Agent(s, zeros(Int, memory_size), newborn_HP, new_ID())
Agent(p::Agent) = Agent(p.strategy)

decision(a, id)::ChoiceOrNot = a.strategy.decision(a.memory, id)
inform(a, c) = a.strategy.learning(a.memory, c)

fertility(a::Agent)::Int = div(a.HP, HP_per_child)
starving(a::Agent)::Bool = a.HP ≤ starving_HP

function versus(a, b)
	if starving(a) || starving(b)
		return
	end

	choice_a = decision(a, b.ID)
	choice_b = decision(b, a.ID)

	a.HP += payoff(choice_a, choice_b)
	b.HP += payoff(choice_b, choice_a)

	inform(a, choice_b)
	inform(b, choice_a)
end

τ = 30

function year(agents::Array{Agent})
	for (a, b) in shuffle(repeat(collect(combinations(agents, 2)), τ))
		versus(a, b)
	end
	newborns = [Agent(parent) for parent in agents for i in 1:fertility(parent)]
	overcapacity = length(newborns) - capacity
	if overcapacity > 0
		selection = trues(length(newborns))
		selection[1:overcapacity] .= false
		newborns = newborns[shuffle(selection)]
	end
	return newborns
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
						   if length(agents) ≤ 1
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
