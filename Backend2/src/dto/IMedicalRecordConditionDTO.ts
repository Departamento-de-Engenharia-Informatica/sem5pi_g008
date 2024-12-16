import IMedicalConditionDTO from "./IMedicalConditionDTO";

export default interface IMedicalRecordConditionDTO{
  condition: IMedicalConditionDTO;
  doctorId: string;
  comment: string;
}
