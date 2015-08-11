//----------------------------------------------------------------------
//                  Find the k Nearest Neighbors
// File:                    kNNdist.cpp
//----------------------------------------------------------------------
// Copyright (c) 2015 Michael Hahsler. All Rights Reserved.
//
// This software is provided under the provisions of the
// GNU General Public License (GPL) Version 3
// (see: http://www.gnu.org/licenses/gpl-3.0.en.html)


#include <Rcpp.h>
#include "ANN/ANN.h"

using namespace Rcpp;

// returns distance to the k-th nearest neighbor
// [[Rcpp::export]]
NumericVector kNNdist_int(NumericMatrix data, int k,
  int type, int bucketSize, int splitRule, double approx) {

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

  // create kd-tree (1) or linear search structure (2)
  ANNpointSet* kdTree = NULL;
  if (type==1){
    kdTree = new ANNkd_tree(dataPts, nrow, ncol, bucketSize,
      (ANNsplitRule)  splitRule);
  } else{
    kdTree = new ANNbruteForce(dataPts, nrow, ncol);
  }
  //Rprintf("kd-tree ready. starting DBSCAN.\n");

  // kNN distance (use k+1 since the search also returns the query point)
  std::vector<double> d(nrow, 0);
  ANNdistArray dists = new ANNdist[k+1];
  ANNidxArray nnIdx = new ANNidx[k+1];

  for (int i=0; i<nrow; i++) {
    ANNpoint queryPt = dataPts[i];

    if(type==1) kdTree->annkSearch(queryPt, k+1, nnIdx, dists, approx);
    else kdTree->annkSearch(queryPt, k+1, nnIdx, dists);

    // take sqrt of largest distance since the tree stores d^2
    d[i] = pow(*std::max_element(dists, dists+k), .5);
  }

  // cleanup
  delete kdTree;
  delete [] dists;
  delete [] nnIdx;
  annDeallocPts(dataPts);
  annClose();

  return wrap(d);
}

// returns knn + dist
// [[Rcpp::export]]
List kNN_int(NumericMatrix data, int k,
  int type, int bucketSize, int splitRule, double approx) {

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

  // create kd-tree (1) or linear search structure (2)
  ANNpointSet* kdTree = NULL;
  if (type==1){
    kdTree = new ANNkd_tree(dataPts, nrow, ncol, bucketSize,
      (ANNsplitRule)  splitRule);
  } else{
    kdTree = new ANNbruteForce(dataPts, nrow, ncol);
  }
  //Rprintf("kd-tree ready. starting DBSCAN.\n");

  NumericMatrix d(nrow, k);
  IntegerMatrix id(nrow, k);

  // Note: the search also returns the point itself (a the first hit)!
  // So we have to look for k+1 points.
  ANNdistArray dists = new ANNdist[k+1];
  ANNidxArray nnIdx = new ANNidx[k+1];

  for (int i=0; i<nrow; i++) {
    if (!(i % 100)) Rcpp::checkUserInterrupt();

    ANNpoint queryPt = dataPts[i];

    if(type==1) kdTree->annkSearch(queryPt, k+1, nnIdx, dists, approx);
    else kdTree->annkSearch(queryPt, k+1, nnIdx, dists);

    d(i, _) = sqrt(NumericVector(dists+1, dists+k+1));
    id(i, _) = IntegerVector(nnIdx+1, nnIdx+k+1) + 1;
  }

  // cleanup
  delete kdTree;
  delete [] dists;
  delete [] nnIdx;
  annDeallocPts(dataPts);
  annClose();

  // prepare results
  List ret;
  ret["dist"] = d;
  ret["id"] = id;
  ret["k"] = k;
  return ret;
}
