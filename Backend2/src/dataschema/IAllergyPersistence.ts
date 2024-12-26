export interface IAllergyPersistence {
  domainId: number;
  code: string;
  designation: string;
  description: string;
  effects: string[];
  isDeleted: boolean;
}
