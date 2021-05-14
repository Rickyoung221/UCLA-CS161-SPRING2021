# Project 2

主要4题，这次目的是深化用lisp实现一些算法比如二叉树的BFS广度遍历和DFS深度遍历

然后尝试用lisp去解决谜题，也即是食人族和传教士过河题。一个简单的入门门人工智能解决。基本思想就是设立约束条件，每一步步骤里都把错误不符合要求的选项都排除，那么每一步只剩下正确的步骤。全部步骤合起来就是所求的path.

分数 96.6/100:

hw2 test failed on the following test case:
```lisp
(MC-DFS '(3 2 NIL) '((0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T)))
```
