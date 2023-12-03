:- module(presupUnittest,[checkTypeTest/4,
                          resolvePresupTest/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type-checking test %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% anaphora1.txt
% John loves his mother.
checkTypeTest(1,[[[[z1,entity],[po(u,1),mother(z1)]],@(14,entity)]],[],exists(z1,and(and(mother(z1),argof(z1,@(14,entity))),exists(v,and(and(love(v),eq(subj(v),john)),eq(acc(v),z1)))))).

% anaphora2.txt
% Some student loves his friend.
checkTypeTest(2,[[[[x,entity],[po(u,1),student(x)],[z1,entity],[po(u,2),friend(z1)]],@(52,entity)]],[],exists(x,and(student(x),exists(z1,and(and(friend(z1),argof(z1,@(52,entity))),exists(v,and(and(love(v),eq(subj(v),x)),eq(acc(v),z1)))))))).

% anaphora3.txt
% Every cat chased its tail.
checkTypeTest(3,[[[[x,entity],[po(u,1),cat(x)],[z1,entity],[po(u,2),tail(z1)]],@(5,entity)]],[],forall(x,imp(cat(x),exists(z1,and(and(tail(z1),argof(z1,@(5,entity))),exists(v,and(and(chase(v),eq(subj(v),x)),eq(acc(v),z1)))))))).

% anaphora4.txt
% Some student walks and he whistles.
checkTypeTest(4,[[[[po(u,3),exists(x,entity,and(student(x),exists(v,event,and(walk(v),eq(subj(v),x)))))],[v,event],[po(u,4),whistle(v)]],@(85,entity)]],[],and(exists(x,and(student(x),exists(v,and(walk(v),eq(subj(v),x))))),exists(v,and(whistle(v),eq(subj(v),@(85,entity)))))).

% anaphora5.txt
% Every student knows that his father comes with her friend.
checkTypeTest(5,[[[[x,entity],[po(u,1),student(x)],[v,event],[po(u,3),and(know(v),eq(subj(v),x))],[x,entity],[po(u,4),father(x)]],@(62,entity)],[[[x,entity],[po(u,1),student(x)],[v,event],[po(u,3),and(know(v),eq(subj(v),x))],[x,entity],[po(u,5),and(father(x),argof(x,@(62,entity)))],[v,event],[po(u,7),and(come(v),eq(subj(v),x))],[x,entity],[po(u,8),friend(x)]],@(21,entity)]],[],forall(x,imp(student(x),exists(v,and(and(know(v),eq(subj(v),x)),acci(v,exists(x,and(and(father(x),argof(x,@(62,entity))),exists(v,and(and(come(v),eq(subj(v),x)),exists(x,and(and(friend(x),argof(x,@(21,entity))),with(v,x))))))))))))).

% anaphora6.txt
% Some student came.
checkTypeTest(6,[],[],exists(x,and(student(x),exists(v,and(come(v),eq(subj(v),x)))))).

% anaphora7.txt
% He came.
checkTypeTest(7,[[[[v,event],[po(u,1),come(v)]],@(59,entity)]],[],exists(v,and(come(v),eq(subj(v),@(59,entity))))).

% anaphora8.txt
% He walks and she runs.
checkTypeTest(8,[[[[v,event],[po(u,1),walk(v)]],@(15,entity)],[[[po(u,2),exists(v,event,and(walk(v),eq(subj(v),@(15,entity))))],[v,event],[po(u,3),run(v)]],@(93,entity)]],[],and(exists(v,and(walk(v),eq(subj(v),@(15,entity)))),exists(v,and(run(v),eq(subj(v),@(93,entity)))))).

% anaphora9.txt
% John loves the girl.
checkTypeTest(9,[[[[v,event],[po(u,2),and(love(v),eq(subj(v),john))]],@(72,exists(z1,entity,girl(z1)))]],[],exists(v,and(and(love(v),eq(subj(v),john)),eq(acc(v),pi1(@(72,exists(z1,girl(z1)))))))).


% anaphora10.txt
% Some student came in and the student whistled.
checkTypeTest(10,[[[[po(u,4),exists(x,entity,and(student(x),exists(v,event,and(and(come(v),eq(subj(v),x)),in(v)))))],[v,event],[po(u,5),whistle(v)]],@(12,exists(x,entity,student(x)))]],[],and(exists(x,and(student(x),exists(v,and(and(come(v),eq(subj(v),x)),in(v))))),exists(v,and(whistle(v),eq(subj(v),pi1(@(12,exists(x,student(x))))))))).

% anaphora11.txt
% The cat chased the dog.
checkTypeTest(11,[[[[v,event],[po(u,1),chase(v)]],@(2,exists(x,entity,cat(x)))],[[[v,event],[po(u,2),and(chase(v),eq(subj(v),pi1(@(2,exists(x,entity,cat(x))))))]],@(99,exists(z1,entity,dog(z1)))]],[],exists(v,and(and(chase(v),eq(subj(v),pi1(@(2,exists(x,cat(x)))))),eq(acc(v),pi1(@(99,exists(z1,dog(z1)))))))).

% anaphora12.txt
% Some professor loves the student he once taught.
checkTypeTest(12,[[[[x,entity],[po(u,1),professor(x)],[v,event],[po(u,3),and(love(v),eq(subj(v),x))],[z1,entity],[po(u,4),student(z1)],[v,event],[po(u,5),teach(v)]],@(16,entity)],[[[x,entity],[po(u,1),professor(x)],[v,event],[po(u,3),and(love(v),eq(subj(v),x))]],@(63,exists(z1,entity,and(student(z1),exists(v,event,and(and(and(teach(v),eq(subj(v),@(16,entity))),eq(acc(v),z1)),once(v))))))]],[],exists(x,and(professor(x),exists(v,and(and(love(v),eq(subj(v),x)),eq(acc(v),pi1(@(63,exists(z1,and(student(z1),exists(v,and(and(and(teach(v),eq(subj(v),@(16,entity))),eq(acc(v),z1)),once(v))))))))))))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Presupposition resolution test %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% anaphora1.txt
% John loves his mother.
resolvePresupTest(1,[exists(t4,entity,exists(t5,exists(z1,entity,and(mother(z1),argof(z1,t4))),exists(v,event,and(and(love(v),eq(subj(v),john)),eq(acc(v),pi1(t5))))))],[],exists(v,and(and(love(v),eq(subj(v),john)),eq(acc(v),pi1(@(59,exists(z1,and(mother(z1),argof(z1,@(50,entity)))))))))).

% anaphora2.txt
% Some student loves his friend.
resolvePresupTest(2,[exists(x,entity,exists(t5,exists(z1,entity,and(friend(z1),argof(z1,x))),and(student(x),exists(v,event,and(and(love(v),eq(subj(v),x)),eq(acc(v),pi1(t5)))))))],[],exists(x,and(student(x),exists(v,and(and(love(v),eq(subj(v),x)),eq(acc(v),pi1(@(78,exists(z1,and(friend(z1),argof(z1,@(44,entity)))))))))))).

% anaphora3.txt
% Every cat chased its tail.
resolvePresupTest(3,[forall(x,entity,exists(t5,exists(z1,entity,and(tail(z1),argof(z1,x))),imp(cat(x),exists(v,event,and(and(chase(v),eq(subj(v),x)),eq(acc(v),pi1(t5)))))))],[],forall(x,imp(cat(x),exists(v,and(and(chase(v),eq(subj(v),x)),eq(acc(v),pi1(@(88,exists(z1,and(tail(z1),argof(z1,@(21,entity)))))))))))).

% anaphora4.txt
% Some student walks and he whistles.
resolvePresupTest(4,[exists(u3,exists(x,entity,and(student(x),exists(v,event,and(walk(v),eq(subj(v),x))))),exists(v,event,and(whistle(v),eq(subj(v),pi1(u3)))))],[],and(exists(x,and(student(x),exists(v,and(walk(v),eq(subj(v),x))))),exists(v,and(whistle(v),eq(subj(v),@(13,entity)))))).

% anaphora6.txt
% Some student came.
resolvePresupTest(6,[exists(x,entity,and(student(x),exists(v,event,and(come(v),eq(subj(v),x)))))],[],exists(x,and(student(x),exists(v,and(come(v),eq(subj(v),x)))))).

% anaphora7.txt
% He came.
resolvePresupTest(7,[exists(t2,entity,exists(v,event,and(come(v),eq(subj(v),t2))))],[],exists(v,and(come(v),eq(subj(v),@(59,entity))))).

% anaphora8.txt
% He walks and she runs.
resolvePresupTest(8,[exists(t4,entity,and(exists(v,event,and(walk(v),eq(subj(v),t4))),exists(v,event,and(run(v),eq(subj(v),t4)))))],[],and(exists(v,and(walk(v),eq(subj(v),@(15,entity)))),exists(v,and(run(v),eq(subj(v),@(93,entity)))))).

% anaphora9.txt
% John loves the girl.
resolvePresupTest(9,[exists(t3,exists(z1,entity,girl(z1)),exists(v,event,and(and(love(v),eq(subj(v),john)),eq(acc(v),pi1(t3)))))],[],exists(v,and(and(love(v),eq(subj(v),john)),eq(acc(v),pi1(@(72,exists(z1,girl(z1)))))))).

% anaphora10.txt
% Some student came in and the student whistled.
resolvePresupTest(10,[exists(u4,exists(x,entity,and(student(x),exists(v,event,and(and(come(v),eq(subj(v),x)),in(v))))),exists(v,event,and(whistle(v),eq(subj(v),pi1(u4)))))],[],and(exists(x,and(student(x),exists(v,and(and(come(v),eq(subj(v),x)),in(v))))),exists(v,and(whistle(v),eq(subj(v),pi1(@(67,exists(x,student(x))))))))).

% anaphora11.txt
% The cat chased the dog.
resolvePresupTest(11,[exists(t4,exists(z1,entity,dog(z1)),exists(t3,exists(x,entity,cat(x)),exists(v,event,and(and(chase(v),eq(subj(v),pi1(t3))),eq(acc(v),pi1(t4))))))],[],exists(v,and(and(chase(v),eq(subj(v),pi1(@(60,exists(x,cat(x)))))),eq(acc(v),pi1(@(15,exists(z1,dog(z1)))))))).

% anaphora12.txt
% Some professor loves the student he once taught.
resolvePresupTest(12,[exists(x,entity,exists(t9,exists(z1,entity,and(student(z1),exists(x1,event,and(and(and(teach(x1),eq(subj(x1),x)),eq(acc(x1),z1)),once(x1))))),and(professor(x),exists(v,event,and(and(love(v),eq(subj(v),x)),eq(acc(v),pi1(t9)))))))],[],exists(x,and(professor(x),exists(v,and(and(love(v),eq(subj(v),x)),eq(acc(v),pi1(@(98,exists(z1,and(student(z1),exists(v,and(and(and(teach(v),eq(subj(v),@(28,entity))),eq(acc(v),z1)),once(v))))))))))))).
