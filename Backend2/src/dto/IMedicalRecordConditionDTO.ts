import IMedicalConditionDTO from "./IMedicalConditionDTO";

export default interface IMedicalRecordConditionDTO{
  domainId?: string;
  conditionId: string;
  medicalRecordId: string;
  doctorId: string;
  comment: string;
}
