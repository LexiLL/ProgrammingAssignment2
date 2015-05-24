makeCacheMatrix <- function(x = matrix()) {
	m = matrix()
	#m是输出结果，matrix
	
	set <- function(y) {
    x <<- y
    m <<- matrix()
  }

   # 返回 x，即需要处理的数据。
   get <- function() 
   x
 
   # 将 solve 的值赋给 m，即缓存处理之后的结果。
   setsolve <- function(solve)
   m <<- solve
   # 常值函数，返回 m，即处理结果。
   getsolve <- function() 
   m

   # 函数 makeCacheMatrix 返回的是一个 list，其中共有四个元素，每个元素都是之前定义过的一个函数。
   list(set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve) 
	    #储存结果m，需要处理的matrix x，缓存处理之后的m 也就是逆矩阵， 最后要返回的处理结果 m
}


## Write a short comment describing this function
# 此函数的输入变量 x 必须是一个 matrix
# matrix(1:6, nrow = 2, ncol = 3)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		#solve(x,b)
		#求矩阵A的逆
		
		m <- x$getsolve()
		#cat(m)
		if(!is.na(m)) {
			# m 不是 NA，说明之前已经计算过 solve了，
			# 直接返回结果
			message("getting cached data")
			return(m)
	    }
		
	    data <- x$get()
	    # 计算 solve 值。
	    m <- solve(data, ...)
	    # 如果x 已经缓存了结果，cachemean 函数会在之前的 if 查询中直接返回，不需要再计算了。
	    x$setsolve(m)
	    # 返回结果
	    m
}
