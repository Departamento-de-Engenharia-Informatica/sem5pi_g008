import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import configAux from "../../configAux.json";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";
import {MedicalRecordFreeTextMap} from "../mappers/MedicalRecordFreeTextMapper";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";
import http from 'http';
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {MedicalConditionNotFoundException} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {MedicalRecordConditionNotFoundException} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {Code} from "../domain/Shared/code";
import {Designation} from "../domain/Shared/designation";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject("MedicalRecordRepo") private medicalRecordRepo:IMedicalRecordRepo,
        @Inject("MedicalRecordAllergyRepo") private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject("AllergyRepo") private allergyRepo: IAllergyRepo,
        @Inject("MedicalRecordFreeTextRepo") private medicalRecordFreeTextRepo: IMedicalRecordFreeTextRepo,
        @Inject("MedicalRecordConditionRepo") private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject("MedicalConditionRepo") private medicalConditionRepo: IMedicalConditionRepo
    ) {}

    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
      const medicalProps = {};
      const medicalRecord = MedicalRecord.create(medicalProps);
      await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }

    public async getAllergies(medicalRecordId:string): Promise<IMedicalRecordAllergyDTO[]> {
        
        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }
      
      const medicalRecordPrivateId = medicalRecord.props._id.toString();

      const medicalRecordAllergies = await this.medicalRecordAllergyRepo.getByMedicalId(medicalRecordPrivateId);
      
      let aux : IMedicalRecordAllergyDTO[] = [];

      for(let i = 0; i < medicalRecordAllergies.length; i++){
        const dto = await MedicalRecordAllergyMapper.toDTO(medicalRecordAllergies[i]);
        aux.push(await this.fixDto(dto));
      }

      return aux;
    }

    private async fixDto(dto: IMedicalRecordAllergyDTO): Promise<IMedicalRecordAllergyDTO> {
      const allergy = await this.allergyRepo.getById(dto.allergy);
      dto.allergy = allergy.designation;
      const doctor = await this.getStaffDetails(dto.doctor);
      dto.doctor = doctor.firstName + " " + doctor.lastName;
      return dto;
    }

    public async getMedicalRecordConditions(medicalRecordId: string): Promise<IMedicalRecordConditionDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);
        
        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }
        
        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getMedicalRecordConditionsWithIds(medicalRecord.props._id.toString());
        
        
        if (medicalRecordConditionList.length === 0) {
            throw new NoMedicalRecordConditionsException();
        }

        const medicalRecordConditionDTOList = [];

        for (const medicalRecordCondition of medicalRecordConditionList) {

            const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

            const condition = await this.medicalConditionRepo.getMedicalConditionByBusinessId(medicalRecordCondition.conditionId);

            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition, condition, medicalRecord.id, staffDetailsDTO);
            
            medicalRecordConditionDTOList.push(medicalConditionDTO);
        }

        return medicalRecordConditionDTOList;
    }

    public async getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const code = Code.create(conditionCode);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByCode(code.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Code.");
        }
        

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());
        

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Code.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);
        
        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    public async getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const designation = Designation.create(conditionDesignation);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByDesignation(designation.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Designation.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Designation.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    private async getStaffDetails(staffId: string): Promise<IStaffDetailsDTO> {
        
        const baseUrl = configAux.Backend1.URL || 'http://localhost:4000';
        
        const url = baseUrl + '/Staff';
        const urlApi = `${url}/by-id/${staffId}`;

        return new Promise((resolve, reject) => {
            http.get(urlApi, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {

                    try {
                        const staff: IStaffDetailsDTO = JSON.parse(data);
                        resolve(staff);
                    } catch (error) {
                        resolve(undefined);
                    }
                });
            }).on('error', (error) => {
                reject(error);
            });
        });
    }

    public async addFreeText(medicalRecord: IMedicalRecordFreeTextDTO): Promise<any> {

        const privateId = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecord.medicalRecordId);
        medicalRecord.medicalRecordId=privateId.props._id;
        
        const medicalRecordDomain=MedicalRecordFreeTextMap.toDomain(medicalRecord);
        await this.medicalRecordFreeTextRepo.save(medicalRecordDomain);

    }

    public async getFreeTexts(medicalRecordId:string): Promise<IMedicalRecordFreeTextDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const medicalRecordPrivateId = medicalRecord.props._id.toString();
        console.log(medicalRecordPrivateId);

        const medicalRecordFreeTexts = await this.medicalRecordFreeTextRepo.getByMedicalId(medicalRecordPrivateId);
        let aux : IMedicalRecordFreeTextDTO[] = [];
        
        for(let i = 0; i < medicalRecordFreeTexts.length; i++){
            const dto = await MedicalRecordFreeTextMap.toDTO(medicalRecordFreeTexts[i]);
            aux.push(await this.fixDtoFreeText(dto));
        }

        return aux;
    }
    private async fixDtoFreeText(dto: IMedicalRecordFreeTextDTO): Promise<IMedicalRecordFreeTextDTO> {
        const doctor = await this.getStaffDetails(dto.doctorId);
        if(!doctor){
            dto.doctorId="unknown";
            return dto;
        }
        dto.doctorId = doctor.firstName + " " + doctor.lastName;
        return dto;
    }

}



