# HTN planning under resource and time constraints in hybrid mines

This repository contains the research and code (Lisp) from my bachelor thesis. (2020-2021)

Title: "HTN planning under resource and time constraints in hybrid mines" <br />
(Unfortunetaly the thesis is written only in German)<br />

If you work with HTN planning and the SHOP3 planner, this repository is for you and might serve as idea impulses and template. However, the repostiory containts older code from 2020, SHOP3 is still the actual version, but has been maintained frequently since then, meaning there is no guarantee that there might be SHOP3 version depending bugs when using the code.<br /><br />
Hierarchical task network (HTN) planning is an approach for automated planning in which the dependency among actions can be given in the form of hierarchically structured networks. HTN planning is subject to planning and logical programming. The actions in HTN planning (compared to classic planning) are decomposed into any number of smaller actions. The very low level actions/tasks are called atoms. In an HTN planner a problem needs to be defined, consisting of a domain (enviromnent and objects) and a set of actions. Then the HTN planner search for a plan until a goal state is reached (termination condition). For example, a very simple plan would be move X from A to B. Depending on how you describe the problem, the HTN planner can also search all combinations/solutions of actions within a range of input variables defined in the domain description. In this thesis the objective is to combinatorial solve a queuing system based on heuristics and several constrains. The constrains include time (duration) and resources (e.g. energy consumption, distance traveled, number of vehicles).<br /><br />

The research utilizes the HTN planner SHOP3: https://github.com/shop-planner/shop3 <br />
SHOP3 documentation: https://shop-planner.github.io <br /><br />

<img width="425" alt="Bildschirmfoto 2024-05-18 um 19 38 18" src="https://github.com/KIlian42/HTN-planning-under-resource-and-time-constraints-in-hybrid-mines/assets/57774167/9da0a98d-30df-4319-bfa5-2631e1f0becc">
<br /><br />
<img width="669" alt="Bildschirmfoto 2024-05-18 um 19 39 03" src="https://github.com/KIlian42/HTN-planning-under-resource-and-time-constraints-in-hybrid-mines/assets/57774167/4e0a8ccd-39a1-461c-83c6-045057a24987">
<br /><br />
<img width="438" alt="Bildschirmfoto 2024-05-18 um 19 37 30" src="https://github.com/KIlian42/HTN-planning-under-resource-and-time-constraints-in-hybrid-mines/assets/57774167/5dbd5815-be0a-4ebd-87a9-60fc14595923">
<br /><br />
<img width="420" alt="Bildschirmfoto 2024-05-18 um 19 37 43" src="https://github.com/KIlian42/HTN-planning-under-resource-and-time-constraints-in-hybrid-mines/assets/57774167/b651d9cc-6d64-433f-ae77-93d8d685b041">
<br /><br />
<img width="443" alt="Bildschirmfoto 2024-05-18 um 19 37 57" src="https://github.com/KIlian42/HTN-planning-under-resource-and-time-constraints-in-hybrid-mines/assets/57774167/16f987af-5d3c-4815-87b9-862441788c0c">