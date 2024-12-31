import {Inject, Service} from "typedi";
import mongoose, {Document, Model} from "mongoose";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionRepo from "../services/IRepos/IMedicalRecordConditionRepo";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import {NotFoundException} from "../Exceptions/NotFoundException";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";


@Service()
export default class MedicalRecordConditionRepo implements IMedicalRecordConditionRepo{
  constructor(@Inject('medicalRecordConditionSchema') private medicalRecordConditionSchema: Model<IMedicalRecordConditionPersistence & Document>,) {
  }



  public async updateUsingDomainId(medicalCondition: any, comment: string): Promise<MedicalRecordCondition> {
    console.log("REPO1")
    const domainId = medicalCondition.domainId;
    const rawMedicalCondition = MedicalRecordConditionMapper.toPersistence(medicalCondition, domainId);
console.log("REPO2")
    delete rawMedicalCondition.domainId;
    delete rawMedicalCondition.doctorId;
    delete rawMedicalCondition.conditionId;
    delete rawMedicalCondition.medicalRecordId;
console.log("REPO3")

    const remainingFields = ['comment'];

    // for (const field of comment) {
    //   console.log("field", field)
    //   if (!remainingFields.includes(field)) {
    //     throw new Error("Invalid field to update");
    //   }
    // }
console.log("REPO4")
    Object.keys(rawMedicalCondition).forEach((key) => {
      if (!comment.includes(key)) {
        delete rawMedicalCondition[key];
      }
    });
console.log("REPO5")
    const updatedMedicalCondition = await this.medicalRecordConditionSchema.findOneAndUpdate(
        {domainId: domainId},
        rawMedicalCondition,
        {new: true}
    );
console.log("REPO6")
    if (!updatedMedicalCondition) {
      throw new NotFoundException("Medical Condition not found");
    }
    console.log("REPO7")

    return MedicalRecordConditionMapper.toDomain(updatedMedicalCondition);
  }
  public async getByDomainId(number: number): Promise<MedicalRecordCondition> {
    const medicalCondition = await this.medicalRecordConditionSchema.findOne({domainId: number});

    if (!medicalCondition) {
      throw new NotFoundException("Medical Condition not found");
    }

    return MedicalRecordConditionMapper.toDomain(medicalCondition);
  }


  public async getMedicalRecordConditionById(s: string): Promise<MedicalRecordCondition> {
    console.log('RepoESTOYRepoESTOYRepoESTOYRepoESTOY');
    const number=Number(s);
    const medicalRecordCondition = await this.medicalRecordConditionSchema.findOne({domainId:number});
    console.log('medicalRecordCondition', medicalRecordCondition);

    return MedicalRecordConditionMapper.toDomain(medicalRecordCondition);
  } 

  
  exists(t: MedicalRecordCondition): Promise<boolean> { 
    //TODO:Implement exists method    
    return Promise.resolve(false);
  }

  public async save(medicalRecordCondition: MedicalRecordCondition): Promise<MedicalRecordCondition> { 

    const id = await this.getLastId();

    const rawMedicalRecordCondition: any = MedicalRecordConditionMapper.toPersistence(medicalRecordCondition, id);

    const medicalRecordConditionCreated = await this.medicalRecordConditionSchema.create(rawMedicalRecordCondition);

    return MedicalRecordConditionMapper.toDomain(medicalRecordConditionCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordConditionSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

  public async getMedicalRecordConditionsWithIds(medicalRecordId: string): Promise<any[]> {
   
      const objectId = new mongoose.Types.ObjectId(medicalRecordId);

      const medicalRecordConditions = await this.medicalRecordConditionSchema.find({ medicalRecordId: objectId });
      
      medicalRecordConditions.map(MedicalRecordConditionMapper.toDomain);
      
      return medicalRecordConditions;
  }

  public async getMedicalRecordConditionByMedicalRecordIdAndConditionId(medicalRecordId: string, medicalConditionCode: string): Promise<any> {
    
    const objectId = new mongoose.Types.ObjectId(medicalRecordId);
    
    const medicalRecordCondition = await this.medicalRecordConditionSchema.findOne({ medicalRecordId: objectId, conditionId : medicalConditionCode });
    
    if(!medicalRecordCondition) {
      return undefined;
    }
    
    return MedicalRecordConditionMapper.toDomain(medicalRecordCondition);
  }

  public async getAllMedicalRecordConditions(): Promise<MedicalRecordCondition[]> {
    const medicalRecordConditions = await this.medicalRecordConditionSchema.find();
    medicalRecordConditions.map(MedicalRecordConditionMapper.toDomain);
    return medicalRecordConditions;
  }

}
