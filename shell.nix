(import ./.).shellFor {
  packages = p: [ p.BiobaseTypes p.DPutils p.ForestStructures p.OrderedBits p.PrimitiveArray p.SciBaseTypes ];
  withHoogle = true;
}
