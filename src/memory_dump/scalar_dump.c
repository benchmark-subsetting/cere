//~ /*Only dumping int and double scalar*/
//~ void write_bin_file(char *path, char* name, char *type, void *adrr) {
    //~ char scalar_path[256];
    //~ char scalar_file[256];
    //~ if(!strcmp(type, "i32*")) {
        //~ snprintf(scalar_path, sizeof(scalar_path), "%s/%s", path, "integers");
        //~ snprintf(scalar_file, sizeof(scalar_file), "%s/%s", scalar_path, name);
        //~ mkdir(scalar_path, 0777);
//~ 
        //~ FILE *scalar = fopen(strcat(scalar_file, ".bin"), "a");
        //~ if (!scalar) {
            //~ fprintf(stderr, "Could not create scalar file");
            //~ exit(-1);
        //~ }
        //~ fwrite((const void*)(adrr), sizeof(int), 1, scalar);
        //~ fclose(scalar);
    //~ }
    //~ else if(!strcmp(type, "double*")) {
        //~ snprintf(scalar_path, sizeof(scalar_path), "%s/%s", path, "doubles");
        //~ snprintf(scalar_file, sizeof(scalar_file), "%s/%s", scalar_path, name);
        //~ mkdir(scalar_path, 0777);
//~ 
        //~ FILE *scalar = fopen(strcat(scalar_file, ".bin"), "a");
        //~ if (!scalar) {
            //~ fprintf(stderr, "Could not create scalar file");
            //~ exit(-1);
        //~ }
        //~ fwrite((const void*)(adrr), sizeof(double), 1, scalar);
        //~ fclose(scalar);
    //~ }
//~ }

/*
  void * addresses[count];
  char *type, *name;
  va_list ap;
  int j;
  va_start(ap, count); 
  for(j=0; j<count; j++) {
      addresses[j] = va_arg(ap, void*);
      /*Only to dump scalars. Will be removed*/
      //~ type = va_arg(ap, char*); //retrieve the type
      //~ name = va_arg(ap, char*); //retrieve the name
      //~ write_bin_file(path, name, type, addresses[j]);
  }
  va_end(ap);
*/

