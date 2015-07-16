#include <Rcpp.h>
#include "ANN/ANN.h"

using namespace Rcpp;

std::vector<int> regionQuery(int id, ANNpointArray dataPts, ANNkd_tree* kdTree,
  double eps2, double approx) {

  // find fixed radius nearest neighbors
  ANNpoint queryPt = dataPts[id];
  std::vector<int> ret = kdTree->annkFRSearch2(queryPt, eps2, 0, NULL, NULL, approx);
  std::sort(ret.begin(), ret.end());

  return(ret);
}


// [[Rcpp::export]]
IntegerVector dbscan_int(NumericMatrix data, double eps, int minPts,
  int bucketSize, int splitRule, double approx) {

  double eps2 = eps*eps;

  // copy data
  int nrow = data.nrow();
  int ncol = data.ncol();
  ANNpointArray dataPts = annAllocPts(nrow, ncol);
  for(int i = 0; i < nrow; i++){
    for(int j = 0; j < ncol; j++){
      (dataPts[i])[j] = data(i, j);
    }
  }

  //Rprintf("Points copied.\n");

  // create kd-tree
  ANNsplitRule sRule = (ANNsplitRule)  splitRule;
  ANNkd_tree* kdTree = new ANNkd_tree(dataPts, nrow, ncol,
    bucketSize, sRule);

  //Rprintf("kd-tree ready. starting DBSCAN.\n");

  // DBSCAN
  std::vector<bool> visited(nrow, false);
  std::vector< std::vector<int> > clusters; // vector of vectors == list
  std::vector<int> noise;

  for (int i=0; i<nrow; i++) {
    //Rprintf("processing point %d\n", i);

    if (visited[i]) continue;

    std::vector<int> N = regionQuery(i, dataPts, kdTree, eps2, approx);
    if((int) N.size() < minPts) {
      // noise points stay unassigned
    } else {

      // start new cluster and expand
      std::vector<int> cluster;
      cluster.push_back(i);
      visited[i] = true;

      while(!N.empty()) {
        int j = *N.begin();
        //int j = *N.end();
        if(visited[j]) {
          N.erase(N.begin());
          //N.pop_back();
          continue; // point already processed
        }

        visited[j] = true;
        std::vector<int> N2 = regionQuery(j, dataPts, kdTree, eps2, approx);
        if((int) N2.size() >= minPts) { // expand neighborhood
          std::vector<int> dest(N.size() + N2.size());
          std::set_union(N.begin(), N.end(), N2.begin(), N2.end(),
            std::back_inserter(dest));
          N = dest;
        }
        cluster.push_back(j);
      }

      clusters.push_back(cluster);
    }
  }

  // unassigned points are noise (cluster 0)

  // prepare cluster vector
  IntegerVector id(nrow);
  for(std::size_t i=0; i<clusters.size(); i++) {
    for(std::size_t j=0; j<clusters[i].size(); j++) {
      id[clusters[i][j]] = i+1;
    }
  }

  return wrap(id);
}
