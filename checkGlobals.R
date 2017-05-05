
checkGlobals =
function(file, env = new.env())
{
   source(file, env)
   funs = getFunctions(env)

   g = lapply(funs, codetools::findGlobals, FALSE)
   w = sapply(g, function(x) length(x$variables)) > 0
   lapply(g[w], identifyGlobals, env)
}

identifyGlobals =
function(vars, env)
{
#browser()    
   vars = unlist(vars)
   exists = vars %in% ls(env) | sapply(vars, function(x) length(find(x)) > 0)
   names(exists) = vars
   isFun = sapply(vars[exists], function(x) is.function(get(x, env)))
   names(isFun) = vars[exists]

   list(undefined = vars[!exists], globals = unname(vars[exists][!isFun]))
}


getFunctions =
function (env = globalenv()) 
{
    objs = ls(env, all = TRUE)
    funs = lapply(objs, function(x) {
        x = get(x, env)
        if (is.function(x)) 
            x
        else NULL
    })
    names(funs) = objs
    invisible(funs[!sapply(funs, is.null)])
}
