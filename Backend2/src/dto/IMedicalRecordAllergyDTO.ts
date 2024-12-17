import IAllergyDTO from "./IAllergyDTO";

export default interface IMedicalRecordAllergyDTO {
  domainId?: string;
  allergyId: string;
  medicalRecordId: string;
  doctorId: string;
  comment: string;
}
