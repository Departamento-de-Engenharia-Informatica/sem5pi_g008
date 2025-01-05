export default interface IAllergyDTO {
  domainId?: number;
  code: string;
  designation: string;
  description: string;
  effects: string[];
  isDeleted?: boolean;
}
