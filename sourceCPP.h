#include <Rcpp.h>
#include<math.h>
using namespace Rcpp;
// [[Rcpp::export]]
float getDistance(std::vector<int>  query,std::vector<int>  db)
{
	int n1 = query.size();
    int n2 = db.size();
	int i = 0, j =0;
	int count = 0;
    while(i <= n1 && j <= n2)
    {
        if(query[i] > db[j])
        {
            j++;
        }
        else if(db[j] > query[i])
        {
            i++;
        }
        else
        {
			count++;
            i++;
            j++;
        }
    }
	float x = count/(sqrt(n1)*sqrt(n2));
	//Rcout<<count<<" "<<n1<<" "<<n2;
	if (x < -1.0) x = -1.0 ;
	else if (x > 1.0) x = 1.0 ;
	return acos (x);
}
// [[Rcpp::export]]
NumericVector stl_sort(NumericVector x) {
   NumericVector y = clone(x);
   std::sort(y.begin(), y.end());
   return y;
}
// [[Rcpp::export]]
float getDistance_1(NumericVector query,NumericVector db)
{
	Rcout<<"Hello";
	std::sort(query.begin(), query.end());
	std::sort(db.begin(), db.end());
	std::vector<int>  q = Rcpp::as<std::vector<int> >(stl_sort(query));
	std::vector<int>  d = Rcpp::as<std::vector<int> >(stl_sort(db));
	return getDistance(q,d);
}
// [[Rcpp::export]]
NumericVector getNearest(List query, List db) {
  Rcpp::List xl(db);
  Rcpp::List query_list(query);
  int query_length = query_list.size();
  int n = xl.size();
  NumericVector ret(query_length);
  NumericVector min_dist(query_length);
  for( int i =0;i<query_length;i++)
  {	
	ret[i] = -1;
	min_dist[i] = 99;
  }
  int ll;
  for(int k = 0; k < n; ++k) 
  {
  	std::vector<int> u = Rcpp::as<std::vector<int> >(xl[k]);
	int n2 = u.size();
	if(n2 > 0)
	{
		for(int m = 0;m<query_length;m++)
		{
			std::vector<int>  q = Rcpp::as<std::vector<int> >(query_list[m]);
/*			int n1 = q.size();		
			int i = 0, j =0;
			int count = 0;
			while(i <= n1 && j <= n2)
			{
				if(q[i] > u[j])
				{
					j++;
				}
				else if(u[j] > q[i])
				{
					i++;
				}
				else
				{
					count++;
					i++;
					j++;
				}
			}
*/
			//float x =  count/(sqrt(n1)*sqrt(n2));
			//if (x < -1.0) x = -1.0 ;
			//else if (x > 1.0) x = 1.0 ;
			float dist = getDistance(q,u);
			if(dist < min_dist[m])
			{
				min_dist[m] = dist;
				ret[m] = k;
			}
		}
	}
	ll=k;
  }
  for( int i =0;i<query_length;i++)
  {	
	ret[i] = ret[i]+1001;
  }
  
  return min_dist;
}

// [[Rcpp::export]]
NumericVector getNearest_Advancement(List query, List db,int approx) {
  Rcpp::List xl(db);
  Rcpp::List query_list(query);
  int query_length = query_list.size();
  int n = xl.size();
  NumericVector ret(query_length);
  NumericVector min_dist(query_length);
  NumericVector q_list(query_length);
  for( int i =0;i<query_length;i++)
  {	
	ret[i] = -1;
	min_dist[i] = 99;
	q_list[i] = 1;
  }
  int cur_r_length = Rcpp::as<std::vector<int> >(xl[0]).size();
  int pre_r_length = cur_r_length;
  for(int k = 0; k < n; ++k) 
  {
  	std::vector<int> u = Rcpp::as<std::vector<int> >(xl[k]);
	int n2 = u.size();
	if(n2 > 0)
	{
		pre_r_length = cur_r_length;
		cur_r_length = n2;
		for(int m = 0;m<query_length;m++)
		{
			if(	q_list[m] == 1)
			{
				std::vector<int>  q = Rcpp::as<std::vector<int> >(query_list[m]);
				int n1 = q.size();	
				if(cur_r_length>pre_r_length)
				{
					//Rcout<<cur_r_length<<"\n";
					if(n1 < cur_r_length)
					{
						//Rcout<<n1<<" "<< cur_r_length<<" "<<sqrt(n1)<<" "<< sqrt(cur_r_length) << " "<<(n1/(sqrt(n1)*sqrt(cur_r_length)))*(1+approx)<<" "<<min_dist[m]<<"\n";
						if(acos(n1/(sqrt(n1)*sqrt(cur_r_length)))*(1+approx)>min_dist[m])
						{
							q_list[m] = 0;
							//Rcout<<m<<"\n";
						}
					}
				}
	/*			int i = 0, j =0;
				int count = 0;
				while(i <= n1 && j <= n2)
				{
					if(q[i] > u[j])
					{
						j++;
					}
					else if(u[j] > q[i])
					{
						i++;
					}
					else
					{
						count++;
						i++;
						j++;
					}
				}
				float x = count/(sqrt(n1)*sqrt(n2));
	*/
				//if (x < -1.0) x = -1.0 ;
				//else if (x > 1.0) x = 1.0 ;
				float dist = getDistance(q,u);
				if(dist < min_dist[m])
				{
					min_dist[m] = dist;
					ret[m] = k;
				}
			}
		}
	}
  }
  return min_dist;
}




