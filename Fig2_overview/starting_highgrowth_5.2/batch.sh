########################

#sed -e "s/linkage/0.0/" -e "s/dom/0.0/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_rec_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_rec_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/0.0/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_rec_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_rec_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/0.5/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_co_unlinked.dat &&  tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_co_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/0.5/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_co_linked.dat &&    tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_co_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/1.0/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_dom_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_dom_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/1.0/" -e "s/male/T/" -e "s/fem/T/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fy_dom_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fy_dom_linked.dat && rm Bas*.dat

#######################

#sed -e "s/linkage/0.0/" -e "s/dom/0.0/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_rec_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_rec_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/0.0/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_rec_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_rec_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/0.5/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_co_unlinked.dat &&  tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_co_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/0.5/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_co_linked.dat &&    tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_co_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/1.0/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_dom_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_dom_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/1.0/" -e "s/male/T/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > my_fn_dom_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> my_fn_dom_linked.dat && rm Bas*.dat

########################

#sed -e "s/linkage/0.0/" -e "s/dom/0.0/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > mn_fn_rec_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_rec_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/0.0/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > mn_fn_rec_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_rec_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/0.5/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > mn_fn_co_unlinked.dat &&  tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_co_unlinked.dat && rm Bas*.dat

sed -e "s/linkage/1.0/" -e "s/dom/0.5/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
head -n 17 *_1000*.dat > mn_fn_co_linked.dat &&    tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_co_linked.dat && rm Bas*.dat

#sed -e "s/linkage/0.0/" -e "s/dom/1.0/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > mn_fn_dom_unlinked.dat && tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_dom_unlinked.dat && rm Bas*.dat

#sed -e "s/linkage/1.0/" -e "s/dom/1.0/" -e "s/male/F/" -e "s/fem/F/" params.dat > Temporary_Input.dat && ./BMS_V5.2
#head -n 17 *_1000*.dat > mn_fn_dom_linked.dat &&   tail -n +18 *_1000*.dat | shuf -n 10000 >> mn_fn_dom_linked.dat && rm Bas*.dat




