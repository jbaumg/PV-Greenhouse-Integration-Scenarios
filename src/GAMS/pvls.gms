set t;

set c;

set d;

set dt;

set tech_param;

parameter demand(t);

parameter controllable_demand(dt,d);

parameter pv_production(t);

parameter costs;

parameter t_in_d(dt, d, t);

parameter max_power_controllable_load(dt);

parameter technical_parameters(tech_param);

parameter max_power_to_grid;

$gdxin data/input.gdx

$load t c d dt tech_param demand pv_production costs controllable_demand t_in_d technical_parameters max_power_controllable_load max_power_to_grid
$gdxin

display controllable_demand, t_in_d;

positive variable
x_pv,
x_storage,
x_in,
x_out,
x_soc,
x_direct_use,
x_curtailment,
x_buy_from_grid,
x_sell_to_grid,
x_control_demand;

variable x_cost;

equations
obj,
demand_balance,
pv_balance,
stor_max,
stor_balance,
control_dem,
control_dem_max,
max_cap_pv,
max_power_to_grid;

obj..x_cost =E= costs("investment_costs_PV") * x_pv +
                costs("investment_costs_storage") * x_storage +
                sum(t, costs("grid_buy_costs") * x_buy_from_grid(t)) -
                sum(t, costs("grid_sell_price") * x_sell_to_grid(t))
                        ;

demand_balance(t).. x_direct_use(t) + x_out(t) + x_buy_from_grid(t)=E=
                            demand(t) + SUM(dt, x_control_demand(dt,t)) + x_sell_to_grid(t);

pv_balance(t)..pv_production(t)*x_pv =E= x_direct_use(t) +
                                         x_in(t) +
                                         x_curtailment(t);

stor_max(t).. x_soc(t) =L= x_storage;

stor_balance(t)..x_soc(t) =E= x_soc(t-1) + technical_parameters("efficiency_storage")*x_in(t) - x_out(t);

control_dem(dt,d)..controllable_demand(dt,d) =E= SUM(t$t_in_d(dt,d,t), x_control_demand(dt,t));

control_dem_max(t, dt)..x_control_demand(dt,t) =L= max_power_controllable_load(dt);


max_power_to_grid_eq(t).. x_sell_to_grid(t) =L= technical_parameters("max_power_to_grid");

max_cap_pv..x_pv =L= 1000000;

model pv /all/;

solve pv minimizing x_cost using lp;

scalar modelstat, solvestat;
modelstat =  pv.modelstat;
solvestat =  pv.solvestat;

Execute_Unload 'path/output.gdx';
Execute_Unload 'path/%site%/%shift%/%namesc%out.gdx';












