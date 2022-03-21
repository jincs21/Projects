SELECT *
FROM Covid..CovidVaccinations
where continent is not null
order by 3,4


--SELECT *
--FROM Covid..CovidDeaths
--order by  3,4



Select Location, date, total_cases, new_cases, total_deaths, population
FROM Covid..CovidDeaths
order by 1,2

-- We look at proportion of deaths in each country
--We can check the probability of dying as a result of contracting COVID
Select Location, date, total_cases,  total_deaths, (total_deaths / total_cases)*100 as Death_percentage
FROM Covid..CovidDeaths
Where location like '%Moldova%'
order by 1,2

--Now, we look at proportion of infected
Select Location, date, total_cases, population, (total_cases / population)*100 as Infected_proportion
FROM Covid..CovidDeaths
Where location like '%Moldova%'
order by 1,2

--Checking countries with highest infection rate
Select Location, population, MAX(total_cases) as Highest_infection_count, MAX((total_cases / population))*100 as Infected_proportion
FROM Covid..CovidDeaths
--Where location like '%Moldova%'
Group by population, location
order by Infected_proportion desc

--We now show countries with highest death count per population
Select Location, MAX(cast(total_deaths as int)) as TotalDeaths
FROM Covid..CovidDeaths
--Where location like '%Moldova%'
where continent is not null
Group by location
Order by TotalDeaths desc

-- WORLD NUMBERS
	Select date, SUM(new_cases) as Global_new_cases,  SUM(cast(new_deaths as int)) as global_new_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as Death_percentage
	FROM Covid..CovidDeaths
	--Where location like '%Moldova%'
	where continent is not null
	Group by date
	order by 1,2


-- Checking proportion of vaccinations on a global scale

--USE CTE

With popvsvac ( Continent, location, date, population, new_vaccinations, Vaccinations_aggregate)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(Convert(bigint, vac.new_vaccinations)) OVER (Partition by dea.location order by 
dea.location, dea.date) as Vaccinations_aggregate 
FROM Covid..CovidDeaths dea
JOIN Covid..CovidVaccinations vac
On dea.location = vac.location
and dea.date = vac.date
Where dea.continent is not null
--order by 2,3
)
Select *, (Vaccinations_aggregate/population)*100
From popvsvac