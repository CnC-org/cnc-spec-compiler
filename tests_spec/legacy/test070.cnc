//Priority is too big!

//Is correct: No?
(Work)priority=10000000000000000000000000000000;
(Start)priority=0;
[type item<int>: int n];
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
(Work) -> [item];
[item] -> env; 