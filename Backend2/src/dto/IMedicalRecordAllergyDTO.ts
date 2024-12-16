import IAllergyDTO from "./IAllergyDTO";

export default interface IMedicalRecordAllergyDTO {
  allergy: IAllergyDTO;
  doctorId: string;
  comment: string;
}
