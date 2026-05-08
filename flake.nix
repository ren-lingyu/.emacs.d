{
  
  description = "aRenCoco's Emacs configuration";
  
  outputs = { self }: {
    
    homeManagerModules = rec {
      
      emacsConfig = { emacsConfigDir } : { config, lib, pkgs, ... }: {
        
        home.file = builtins.listToAttrs [
          {
            name = "${emacsConfigDir}/early-init.el";
            value.source = "${self}/early-init.el";
          }
          {
            name = "${emacsConfigDir}/init.el";
            value.source = "${self}/init.el";
          }
          {
            name = "${emacsConfigDir}/lisp";
            value = { source = "${self}/lisp"; recursive = true; };
          }
        ];
        
      };

      default = emacsConfig { emacsConfigDir = ".emacs.d" };
      
    };
    
  };
  
}
