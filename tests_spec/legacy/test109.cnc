//Deallocation

//Is correct: No

(Work);
(Start);
[type item<int>: int n]dealloc_func=100;
[type item2<int>: int n]dealloc_func=200;
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
