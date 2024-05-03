--- a/madgraph/loop/loop_diagram_generation.py  2016-11-04 10:49:37.000000000 +0100
+++ b/madgraph/loop/loop_diagram_generation.py   2016-11-16 10:14:30.993495000 +0100
@@ -384,7 +384,7 @@
         # By default the user filter does nothing if filter is not set,
         # if you want to turn it on and edit it by hand, then set the
         # variable edit_filter_manually to True
-        edit_filter_manually = False
+        edit_filter_manually = True
         if not edit_filter_manually and filter in [None,'None']:
             return
         if isinstance(filter,str) and  filter.lower() == 'true':
@@ -414,6 +414,9 @@
                 except Exception as e:
                     raise InvalidCmd("The user-defined filter '%s' did not"%filter+
                                  " returned the following error:\n       > %s"%str(e))
+
+            if any([abs(pdg) not in range(1,7) for pdg in diag.get_loop_lines_pdgs()]) or (23 not in diag.get_pdgs_attached_to_loop(structs)) :
+                valid_diag=False
 #            if any([abs(i)!=1000021 for i in diag.get_loop_lines_pdgs()]):
 #                valid_diag=False
 #            if len(diag.get_loop_lines_pdgs())<4:
