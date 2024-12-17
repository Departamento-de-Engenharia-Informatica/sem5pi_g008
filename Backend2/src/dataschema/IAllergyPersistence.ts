export interface IAllergyPersistence {
  domainId: number;
  allergy: string;
  effect?: string;
  isDeleted: boolean;
}
