node.importance<-function(inputparams,landscape.graph){
  landscape.graph_x<-delete.vertices(landscape.graph,V(landscape.graph)$name[which(as.numeric(V(landscape.graph)$name)==as.numeric(inputparams['Node']))])
  
  x_EC_GAP<-overall.indices.standard(landscape.graph_x,as.numeric(inputparams['coefficientGAP']),inputparams['linkWeight'],inputparams['nodeWeight'])
  x_EC_NATAL<-overall.indices.standard(landscape.graph_x,as.numeric(inputparams['coefficientNATAL']),inputparams['linkWeight'],inputparams['nodeWeight'])
  
  dEC_GAP<-100*(as.numeric(inputparams['EC_Gap'])-x_EC_GAP)/as.numeric(inputparams['EC_Gap'])
  dEC_NATAL<-100*(as.numeric(inputparams['EC_Natal'])-x_EC_NATAL)/as.numeric(inputparams['EC_Natal'])
  
  noderemove_results<-c(Node=inputparams['Node'], dEC_GAP=dEC_GAP, dEC_NATAL=dEC_NATAL)
  
  return(noderemove_results)
}

########################################################################
#     functions to compute overall indices via the standard method     #
########################################################################

overall.indices.standard<-function(landscape.graph,distanceDecayCoefficient,linkWeight,nodeWeight){
  dist_mat<-shortest.paths(landscape.graph, weights=edge_attr(landscape.graph,linkWeight))
  
  #matrix of dispersal kernel
  kernel_mat<-exp(as.numeric(distanceDecayCoefficient)*dist_mat)
  rm(dist_mat)
  
  #matrix of product node attributes
  attribute_mat1<-as.vector(as.numeric(vertex_attr(landscape.graph,nodeWeight)))
  attribute_mat<-attribute_mat1%*%t(attribute_mat1)
  rm(attribute_mat1)
  
  #matrix of PCvalues
  PC_mat<-kernel_mat*attribute_mat
  rm(attribute_mat,kernel_mat)
  
  PCnum<-sum(PC_mat)
  rm(PC_mat)
  EC<-sqrt(PCnum)
  rm(PCnum)
  return(EC)
}s