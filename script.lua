
local io_str = ''

function chance(a)
	return math.random(0, 100) <= a
end

function table.ftabconcat(arr)
	local str = ''
	for _, i in ipairs(arr) do
		local s = string.len(tostring(i))
		str = str .. i
		for q = 1, 7 - s do
			str = str .. ' '
		end
	end
	return str
end

function table.copy(a)
	local b = {}
	for i = 1, #a do
		table.insert(b, a[i])
	end
	return b
end

local Point = {}
local Route = {}

local time = os.clock()
local map = {}
local routes = {}
local new_routes = {}
local best = {
	time = {},
	score = {},
	optimum = {}
}
local min_time = 0xffffffff
local max_score = 0
local __best = {}
local cbest = {}
local recombination_tests = {}

local routes_amount = 25
local test_amount = 1
local gen_amount = 0
local map_size = 100
local limit = 2000
local first_point, last_point = 1, 2

local output = io.open('log/output.lua', 'w')
local process = io.open('log/process.lua', 'w')
local tests = io.open('log/tests.lua', 'w')
local to_excel = io.open('log/to_excel.lua', 'w')

local function dfs(current_point, visited, route)
  table.insert(route, current_point)
  if #route > 4 and current_point == 2 then
		local r = Route.create()
		r.path = table.copy(route)
		r:update_absent()
		table.insert(routes, r)
		--tests:write('\n\n' .. tostring(r))
  else
    for i, point in ipairs(map[current_point].connections) do
      if point ~= 0 and not visited[i] then
        visited[i] = true
        dfs(i, visited, route)
        visited[i] = false
      end
    end
  end
	gen_amount = gen_amount + 1
  table.remove(route)
end

local function update_cbest()
	routes[1].time, routes[1].score = 0, 0
		for _, point in ipairs(routes[1].path) do
			routes[1].time = routes[1].time + map[point].time
			routes[1].score = routes[1].score + map[point].score
		end
		for i = 2, #routes[1].path do
			routes[1].time = routes[1].time + map[routes[1].path[i - 1]].connections[routes[1].path[i]]
		end
	cbest.time = routes[1]:copy()
	cbest.score = routes[1]:copy()
	cbest.optimum = routes[1]:copy()
end

function Point.create() --создать пункт с случайными данными
	local point = {
		time = math.random(10, 50),
		score = math.random(1, 100),
		connections = {}
	}
	setmetatable(point, {__index = Point})
	table.insert(map, point)
	for i = 1, #map do
		point.connections[i] = 0
	end
	for i = 1, #map - 1 do
		table.insert(map[i].connections, 0)
	end
	return point
end

function Route.create() --создать маршрут; не создаёт сам путь
	local route = {
		time = 0xffffffff,
		score = 0,
		optimum = 0,
		path = {},
		absent = {}
	}
	setmetatable(route, {__index = Route, __tostring = function(self)
		return 'time = ' .. self.time .. '\nscore = ' .. self.score .. '\noptimum = ' .. self.optimum .. '\npath = ' .. table.ftabconcat(self.path) .. '\nabsent = ' .. table.ftabconcat(self.absent)
	end})
	local len = math.random(5, #map)
	for i = 1, len do
		table.insert(route.path, 0)
	end
	route.path[1] = first_point
	route.path[len] = last_point
	for i = 1, #map do
		if i ~= first_point and i ~= last_point then
			table.insert(route.absent, i)
		end
	end
	return route
end

function Route:copy()
	local r = Route.create()
	r.path = table.copy(self.path)
	r.absent = table.copy(self.absent)
	r.time, r.score, r.optimum = self.time, self.score, self.optimum
	return r
end

function Route:check_connections() --возвращает true, если какой-то сегмент пути невозможен
	for i = 2, #self.path do
		if map[self.path[i - 1]].connections[self.path[i]] == 0 then return true end
	end
end

function check_connections(path)
	for i = 2, #path do
		if map[path[i - 1]].connections[path[i]] == 0 then return true end
	end
end

function Route:generate_random() --сгенерировать путь
	for i = 2, #self.path - 1 do
		for pos, absent_point in ipairs(self.absent) do
			if map[self.path[i - 1]].connections[absent_point] ~= 0 then
				self.path[i] = absent_point
				table.remove(self.absent, pos)
				break
			end
		end
	end
	for i = 1, map_size do
		self:mutate()
	end
end

function Route:mutate() --мутация пути
	local r
	if 			#self.path == 5 then
		r = math.random(1, 3)
	elseif 	#self.path == #map then
		r = math.random(3, 4)
	else
		r = math.random(1, 4)
	end
	local route, absent = table.copy(self.path), table.copy(self.absent)
	if		 r == 1 then --добавление пункта
		local a = math.random(1, #absent)
		local p = math.random(2, #route - 1)
		table.insert(route, p, absent[a])
		table.remove(absent, a)
	elseif r == 2 then --замена пункта маршрута на тот, которого в нём нет
		local k = math.random(2, #route - 1)
		local a = math.random(1, #absent)
		route[k], absent[a] = absent[a], route[k]
	elseif r == 3 then --замена двух пунктов маршрута друг на друга
		local k1, k2 = 2, 2
		while k1 == k2 do
			k1, k2 = math.random(2, #route - 1), math.random(2, #route - 1)
		end
		route[k1], route[k2] = route[k2], route[k1]
	elseif r == 4 then --удаление пункта маршрута
		local k = math.random(2, #route - 1)
		table.insert(absent, route[k])
		table.remove(route, k)
	end
	--tests:write('\n\nМутация маршрута ' .. r .. '\nИсходный:\npath: ' .. table.ftabconcat(self.path) .. '\nabsent: ' .. table.ftabconcat(self.absent))
	--tests:write('\nНовый:\npath: ' .. table.ftabconcat(route) .. '\nabsent: ' .. table.ftabconcat(absent))
	self.path, self.absent = route, absent
end

function Route:update_absent()
	self.absent = {}
	for i = 1, #map do
		local not_found = true
		for n = 1, #self.path do
			if i == self.path[n] then
				not_found = false break
			end
		end
		if not_found then
			table.insert(self.absent, i)
		end
	end
end

local function recombination() --скрещивание
	local route = Route.create()
	local changed = false
	local __r = 0
	for q = 1, 100 do
		__r = __r + 1
		local max_l = 0
		local a, b = 1, 1
		while a == b do
			a, b = math.random(1, #routes), math.random(1, #routes)
		end
		route.path = table.copy(routes[a].path)
		if #route.path > #routes[b].path then
			max_l = #routes[b].path - 2
		else
			max_l = #route.path - 2
		end
		local a1, b1, d = 1, 1, 0
		a1, b1 = math.random(2, #route.path - max_l), math.random(2, #routes[b].path - max_l)
		d = math.random(2, max_l)
		for i = 0, d - 1 do
			for n = 2, #route.path - 1 do
				if n < a1 or n > a1 + d - 1 then
					if routes[b].path[b1 + i] == route.path[n] then
						--tests:write'\n\nСкрещивание невозможно:'
						--tests:write('\n\nМаршрут 1:\n' .. tostring(routes[a]) .. '\n\nМаршрут 2:\n' .. tostring(routes[b]))
						--tests:write('\n\nУчасток 1: ' .. a1 .. ' - ' .. a1 + d - 1)
						--tests:write('\n\nУчасток 2: ' .. b1 .. ' - ' .. b1 + d - 1)
						goto continue
					end
				end
			end
		end
		for i = 0, d - 1 do
			route.path[a1 + i] = routes[b].path[b1 + i]
		end
		route:update_absent()
		--tests:write'\n\nУспешное скрещивание'
		--tests:write('\n\nМаршрут 1:\n' .. tostring(routes[a]) .. '\n\nМаршрут 2:\n' .. tostring(routes[b]) .. '\n\nРезультат:\n' .. tostring(route))
		--tests:write('\n\na1, a2: ' .. a1 .. ', ' .. a1 + d .. '\nb1, b2: ' .. b1 .. ', ' .. b1 + d .. '')
		changed = true
		break
		::continue::
	end
	recombination_tests[__r] = recombination_tests[__r] + 1
	if changed then
		table.insert(new_routes, route)
	else
		route:generate_random()
		table.insert(new_routes, route)
	end
end

local function calculate_routes() --рассчёт показателей для всех маршрутов и определение лучших
	for _, route in ipairs(routes) do
		route.time, route.score = 0, 0
		for _, point in ipairs(route.path) do
			route.time = route.time + map[point].time
			route.score = route.score + map[point].score
		end
		for i = 2, #route.path do
			route.time = route.time + map[route.path[i - 1]].connections[route.path[i]]
		end
		if max_score < route.score then
			max_score = route.score
			cbest.optimum.optimum = cbest.optimum.score / max_score + min_time / cbest.time.time
		end
		if min_time > route.time then
			min_time = route.time
			cbest.optimum.optimum = cbest.optimum.score / max_score + min_time / cbest.time.time
		end
	end
	for _, route in ipairs(routes) do
		route.optimum = route.score / max_score + min_time / route.time
	end
end

local function sort_routes() --сортировка маршрутов относительно лучшего
	best.time = table.copy(routes)
	best.score = table.copy(routes)
	best.optimum = table.copy(routes)
	table.sort(best.time, function(a, b) return a.time < b.time end)
	table.sort(best.score, function(a, b) return a.score > b.score end)
	table.sort(best.optimum, function(a, b) return a.optimum > b.optimum end)
	if best.time[1].time < cbest.time.time then cbest.time = best.time[1]:copy() end --print(best.time[1])
	if best.score[1].score > cbest.score.score then cbest.score = best.score[1]:copy() end --print(best.score[1])
	if best.optimum[1].optimum > cbest.optimum.optimum then cbest.optimum = best.optimum[1]:copy() end
end

local function print_routes()
	output:write'\n\nТекущая популяция:\n\n'
	for _, route in ipairs(routes) do
		output:write(tostring(route) .. '\n\n')
	end
end

local function gen(c, do_recombination) --циклическая генерация
	for _, route in ipairs(routes) do
		if chance(c) then
			route:mutate()
		end
	end
	calculate_routes()
	sort_routes()
	if do_recombination then
		local n = math.floor(#routes / 2)
		for i = 1, n do
			table.remove(best.optimum, #best.optimum)
		end
		routes = table.copy(best.optimum)
		for i = 1, n do
			recombination()
		end
		for i = 1, n do
			table.insert(routes, new_routes[i])
		end
		new_routes = {}
		calculate_routes()
		sort_routes()
	end
end

local function bgen() --генерация начальной популяции
	routes = {}
	for i = 1, routes_amount do
		local r = Route:create()
		r:generate_random()
		table.insert(routes, r)
	end
	process:write'\n\nНачальная популяция сгенерирована'
end

local function __at_gen(c, do_recombination) --остановка при нахождении всех оптимальных параметров
	gen_amount = 0
	repeat
		gen(c, do_recombination)
		gen_amount = gen_amount + 1
		if gen_amount >= limit then break end
	until cbest.time.time <= __best.time.time and cbest.score.score >= __best.score.score and cbest.optimum.optimum >= __best.optimum.optimum
end

local function generate_map(points_amount, connection_chance) --создать матрицу расстояний определённого размера, в которой каждый пункт соединён с каждым с определённым шансом
	map = {}
	for i = 1, points_amount do
		Point.create()
	end
	for i = 1, points_amount - 1 do
		for n = i + 1, points_amount do
			if chance(connection_chance) then
				map[n].connections[i] = math.random(7, 30)
				map[i].connections[n] = map[n].connections[i]
			end
		end
	end
	process:write'\n\nМатрица расстояний сгенерирована'
end


generate_map(map_size, 100)

output:write'\n\nМатрица расстояний:\n'
for _, i in ipairs(map) do
	output:write(table.ftabconcat(i.connections) .. '\n\n')
end

io_str = '\r['
for i = 1, test_amount do
	io_str = io_str .. '.'
end
io.write(io_str .. ']')
local q = os.clock()

output:write'\n\nЛучшие показатели:'

if map_size > 10 then
	bgen()
	update_cbest()
	for i = 1, limit * 10 do
		gen(100, false)
		gen_amount = gen_amount + 1
	end
else
	dfs(1, {[1] = true}, {})
	update_cbest()
end
calculate_routes()
sort_routes()

output:write('\n\nЛучшие результаты: time = ' .. cbest.time.time .. ', score = ' .. cbest.score.score .. ', optimum = ' .. math.floor(cbest.optimum.optimum * 1000))
to_excel:write(string.gsub(string.format('%.3f\t%i\t%i\t%i\t%.3f', os.clock() - q, gen_amount,  cbest.time.time, cbest.score.score, cbest.optimum.optimum), '%.', ','))
__best.time = cbest.time
__best.score = cbest.score
__best.optimum = cbest.optimum

for p = 1, test_amount do
	
	for i = 1, 101 do
		recombination_tests[i] = 0
	end
	
	io_str = '\r['
	for i = 1, p do
		io_str = io_str .. '#'
	end
	for i = p + 1, test_amount do
		io_str = io_str .. '.'
	end
	io.write(io_str .. ']')
	
	q = os.clock()
	output:write'\n\nТест, только мутация:'
	bgen()
	update_cbest()

	__at_gen(100, false)

	output:write('\n\nЛучшие результаты: time = ' .. cbest.time.time .. ', score = ' .. cbest.score.score .. ', optimum = ' .. math.floor(cbest.optimum.optimum * 1000))
	output:write('\nВремя выполнения: ' .. math.floor((os.clock() - q) * 10000) / 10000 .. ', gen_amount: ' .. gen_amount)
	to_excel:write(string.gsub(string.format('\n%.3f\t%i\t%i\t%i\t%.3f', os.clock() - q, gen_amount, cbest.time.time, cbest.score.score, cbest.optimum.optimum), '%.', ','))


	q = os.clock()
	output:write'\n\nТест, только скрещивание:'
	bgen()
	update_cbest()

	__at_gen(0, true)

	output:write('\n\nЛучшие результаты: time = ' .. cbest.time.time .. ', score = ' .. cbest.score.score .. ', optimum = ' .. math.floor(cbest.optimum.optimum * 1000))
	output:write('\nВремя выполнения: ' .. math.floor((os.clock() - q) * 10000) / 10000 .. ', gen_amount: ' .. gen_amount)
	to_excel:write(string.gsub(string.format('\n%.3f\t%i\t%i\t%i\t%.3f', os.clock() - q, gen_amount, cbest.time.time, cbest.score.score, cbest.optimum.optimum), '%.', ','))

	for g = 10, 100, 10 do
		q = os.clock()
		output:write('\n\nТест, скрещивание и шанс ' .. g .. '% на мутацию:')
		bgen()
		update_cbest()
		
		__at_gen(g, true)

		output:write('\n\nЛучшие результаты: time = ' .. cbest.time.time .. ', score = ' .. cbest.score.score .. ', optimum = ' .. math.floor(cbest.optimum.optimum * 1000))
		output:write('\nВремя выполнения: ' .. math.floor((os.clock() - q) * 10000) / 10000 .. ', gen_amount: ' .. gen_amount)
		to_excel:write(string.gsub(string.format('\n%.3f\t%i\t%i\t%i\t%.3f', os.clock() - q, gen_amount, cbest.time.time, cbest.score.score, cbest.optimum.optimum), '%.', ','))
	end
	output:write('\n\n\t\tЗавершён цикл ' .. p .. '\n\n')
end

tests:write'\n\nТест скрещивания:\n\n'
for i = 1, 101 do
	tests:write(i .. '\t' .. recombination_tests[i] .. '\n')
end


tests:write('\n\nВремя выполнения: ', os.clock() - time)
print'\nDone'

output:close()
process:close()
tests:close()
to_excel:close()

--[[

local r = Route.create()
tests:write(string.format('\n\nПроверка класса Route:\n\ntime = %i, score = %i\npath: %s\nabsent: %s', r.time, r.score, table.ftabconcat(r.path), table.ftabconcat(r.absent)))
r:generate_random()
tests:write(string.format('\n\nПроверка генерации маршрута:\n\ntime = %i, score = %i\npath: %s\nabsent: %s', r.time, r.score, table.ftabconcat(r.path), table.ftabconcat(r.absent)))

tests:write('\n\nТест мутации:\n\npath: ' .. table.ftabconcat(r.path) .. '\nabsent: ' .. table.ftabconcat(r.absent))
r:mutate()
tests:write('\n\npath: ' .. table.ftabconcat(r.path) .. '\nabsent: ' .. table.ftabconcat(r.absent))

tests:write'\n\nТест рассчёта результатов:\n\n'
calculate_routes()
for _, route in ipairs(routes) do
	tests:write(tostring(route) .. '\n\n')
end

tests:write'\n\nТест сортировки результатов:\n'
sort_routes()
tests:write'\tПо времени:\n\n'
for _, route in ipairs(best.time) do
	tests:write(tostring(route) .. '\n\n')
end
tests:write'\tПо счёту:\n\n'
for _, route in ipairs(best.score) do
	tests:write(tostring(route) .. '\n\n')
end
tests:write'\tПо оптимальности:\n\n'
for _, route in ipairs(best.optimum) do
	tests:write(tostring(route) .. '\n\n')
end

]]--